package com.dengage.geofenceengine

import android.annotation.SuppressLint
import android.content.Context
import android.location.Location
import androidx.work.ExistingWorkPolicy
import androidx.work.OneTimeWorkRequestBuilder
import androidx.work.WorkManager
import com.dengage.geofenceengine.storage.GeofenceStorage
import com.dengage.geofenceengine.storage.model.Fence
import com.dengage.geofenceengine.worker.ResumeSlcWorker
import com.dengage.geofence.manager.GeofencePermissionsHelper
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventType
import com.dengage.sdk.util.DengageLogger
import com.google.android.gms.location.LocationServices
import com.google.android.gms.location.Priority
import com.google.android.gms.tasks.CancellationTokenSource
import com.google.android.gms.tasks.Tasks
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import java.util.concurrent.TimeUnit

/**
 * Geofence Engine orchestrator (doc 21 §6.5 `GeofenceEngine`).
 * Sync, reeval, OS register, movement, trigger, wake-up cap ve event flush'ı koordine eder.
 */
@SuppressLint("MissingPermission")
internal class GeofenceEngine(private val context: Context) {

    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)

    private val storage = GeofenceStorage(context)
    private val remoteConfig = RemoteConfigClient()
    private val organicSyncTrigger = OrganicSyncTrigger()
    private val topNSelector = TopNSelector()
    private val registrar = OsGeofenceRegistrar(context)
    private val movementListener = MovementListener(context)
    private val activeWindowScheduler = ActiveWindowScheduler(context)
    private val notificationFirer = LocalNotificationFirer(context)

    private val containmentReconciler = ContainmentReconciler(
        storage.fenceRepository,
        storage.deviceStateRepository
    )
    private val syncer = GeofenceSyncer(storage.fenceRepository, storage.syncMetadataRepository)
    private val heartbeatSender = HeartbeatSender(storage.syncMetadataRepository)
    private val eventFlusher = EventQueueFlusher(storage.eventQueueRepository)
    private val triggerHandler = TriggerHandler(
        context = context,
        fenceRepository = storage.fenceRepository,
        deviceStateRepository = storage.deviceStateRepository,
        eventQueue = storage.eventQueueRepository,
        notificationFirer = notificationFirer,
        eventFlusher = eventFlusher,
        triggerHistory = storage.triggerHistoryRepository,
        configProvider = { remoteConfig.config().offlineQueueMaxSize }
    )

    private val adaptiveThreshold = AdaptiveThresholdCalculator(
        configProvider = { remoteConfig.config().adaptiveThreshold }
    )
    private val wakeupCap = WakeupCapController(
        configProvider = { remoteConfig.config().wakeupCap },
        onPause = { movementListener.stop() },
        onResume = { movementListener.start(remoteConfig.config().reevaluationDistanceMeters) },
        scheduleResume = { minutes -> scheduleResume(minutes) },
        syncMetadata = storage.syncMetadataRepository
    )

    @Volatile
    private var lastReevalLocation: Location? = null

    @Volatile
    private var running = false

    /** Son foreground kaynaklı organik sync anı (İş 5 debounce). */
    @Volatile
    private var lastForegroundSyncAt = 0L

    // ---- lifecycle ----

    fun start() {
        if (!remoteConfig.geofenceEnabled()) {
            DengageLogger.debug("GeofenceEngine -> disabled by server config")
            stop()
            return
        }
        if (!hasLocationPermission()) {
            DengageLogger.debug("GeofenceEngine -> location permission missing")
            return
        }
        running = true

        // Persist edilmiş pause durumunu değerlendir. Süre dolduysa `attemptResume` location
        // update'leri geri açar; dolmadıysa kapalı kalır (ResumeSlcWorker süresi dolunca açacak).
        // Koşulsuz start etmek pause'u sessizce iptal eder → cap'in saatlik sınırı sızar.
        wakeupCap.attemptResume()
        if (wakeupCap.isPaused) {
            DengageLogger.debug("GeofenceEngine -> wake-up cap still paused, location updates stay off")
        } else {
            movementListener.start(remoteConfig.config().reevaluationDistanceMeters)
        }

        scope.launch {
            val location = currentLocation()
            reeval(location, syncAllowed = true, force = true)
        }
    }

    fun stop() {
        running = false
        movementListener.stop()
        registrar.removeAll()
        activeWindowScheduler.cancel()
        adaptiveThreshold.reset()
        wakeupCap.reset()
    }

    fun forceResync() {
        scope.launch {
            val location = currentLocation()
            reeval(location, syncAllowed = true, force = true)
        }
    }

    /**
     * Silent push (sourceType=geofence) ile force resync. Son silent-push zamanını kaydeder ve
     * sunucudan fence'leri yeniden çeker (contract: ad-hoc/garanti senkronizasyon kanalı).
     */
    fun onSilentPush() {
        storage.syncMetadataRepository.lastSilentPushAt = System.currentTimeMillis()
        DengageLogger.debug("GeofenceEngine -> silent push resync")
        forceResync()
    }

    fun lastSilentPushAt(): Long? = storage.syncMetadataRepository.lastSilentPushAt

    fun requestOrganicSync(reason: OrganicSyncTrigger.Reason) {
        if (!remoteConfig.geofenceEnabled()) return
        // Foreground debounce (doc 23 İş 5): her ekran açılışı bir organic sync tetikler; kısa
        // aralıklı foreground'lar (bildirim çekmecesi, app switcher) reeval churn'ü üretmesin.
        // Diff-based register churn'ün en pahalı kısmını zaten kaldırdı; bu, sync/reconcile
        // maliyetini de kırpar. Diğer reason'lar (push delivered / boot / manual) debounce'lanmaz.
        if (reason == OrganicSyncTrigger.Reason.APP_FOREGROUND) {
            val now = System.currentTimeMillis()
            if (now - lastForegroundSyncAt < FOREGROUND_SYNC_DEBOUNCE_MS) {
                DengageLogger.debug("GeofenceEngine -> foreground sync debounced")
                return
            }
            lastForegroundSyncAt = now
        }
        wakeupCap.attemptResume()
        scope.launch {
            val location = currentLocation()
            reeval(location, syncAllowed = true, force = false)
            // Online'a geçmiş olabiliriz: bekleyen offline event'leri flush et
            eventFlusher.flush(remoteConfig.config().offlineQueueMaxSize)
        }
        DengageLogger.debug("GeofenceEngine -> organic sync requested ($reason)")
    }

    fun handlePushDelivered(data: Map<String, String>?): Boolean {
        val shouldSync = organicSyncTrigger.shouldSyncForPush(data)
        if (shouldSync) requestOrganicSync(OrganicSyncTrigger.Reason.PUSH_DELIVERED)
        return shouldSync
    }

    fun onAppForeground() = requestOrganicSync(OrganicSyncTrigger.Reason.APP_FOREGROUND)

    fun attemptResume() = wakeupCap.attemptResume()

    fun onActiveWindowBoundary() {
        scope.launch {
            val location = currentLocation()
            reeval(location, syncAllowed = true, force = false)
        }
    }

    // ---- movement ----

    fun handleMovement(location: Location, onComplete: () -> Unit = {}) {
        if (!remoteConfig.geofenceEnabled()) {
            stop(); onComplete(); return
        }
        when (wakeupCap.recordWakeup()) {
            WakeupAction.SKIP_PAUSED -> { onComplete(); return }
            WakeupAction.PAUSE_AND_SKIP -> { onComplete(); return }
            WakeupAction.PROCEED -> Unit
        }

        scope.launch {
            try {
                heartbeatSender.maybeSend(location, remoteConfig.config().heartbeatIntervalMinutes)

                when (val decision = adaptiveThreshold.shouldReeval(location, lastReevalLocation)) {
                    // Movement kaynaklı → sentetik geçiş kampanya tetikleyebilir (occurredAt=now dürüst).
                    is ReevalDecision.Reeval -> reeval(location, syncAllowed = decision.syncAllowed, force = false, fireCampaigns = true)
                    is ReevalDecision.Skip -> DengageLogger.debug("GeofenceEngine -> reeval skipped (${decision.reason})")
                }
            } finally {
                onComplete()
            }
        }
    }

    // ---- trigger ----

    /**
     * OS transition'ını işler. [onComplete] async ağ işi (event-signal POST) bittiğinde çağrılır;
     * receiver bunu `goAsync().finish()` ile eşler ki arka planda process erken öldürülmesin
     * (yoksa event-signal Doze bakım penceresine ertelenir → push saatlerce gecikir).
     */
    fun handleGeofenceTransition(
        transitionType: Int,
        requestIds: List<String>,
        location: Location?,
        onComplete: () -> Unit = {}
    ) {
        // Region monitoring callback'i pause süresini etkilemez; ayrıca resume fırsatı verir (K13)
        wakeupCap.attemptResume()
        scope.launch {
            try {
                triggerHandler.handleTransition(transitionType, requestIds, location)
                // Cross-fence reconcile. The OS only tells us about the fence it fired for, so
                // entering B never repairs a missed exit from A — A stays INSIDE and dedup swallows
                // every later enter there. This wake is the cheapest repair opportunity we get: the
                // process is already up and the triggering fix is the freshest location available.
                //
                // Must run *after* handleTransition: the reconciler would otherwise synthesize the
                // very transition that woke us, and the real OS callback would then lose to dedup —
                // with campaigns suppressed, silently dropping the event.
                //
                // fireCampaigns=false: a repaired exit happened at an unknown point in the past, so
                // occurredAt=now would be a lie and the push would be stale. Same reasoning as the
                // sync-wake path — fix the state, stay silent.
                location?.let { reconcileContainment(it, fireCampaigns = false) }
            } finally {
                onComplete()
            }
        }
    }

    // ---- diagnostics ----

    /**
     * OS'a en son register edilmiş fence'ler (teşhis). Depodaki listenin tamamı değil — top-N
     * seçimi sonrası gerçekten register edilmiş olanlar.
     */
    fun monitoredGeofences(): List<MonitoredGeofenceInfo> {
        val fencesById = storage.fenceRepository.loadAll().associateBy { it.geofenceId }
        return registrar.registeredFenceRequestIds()
            .mapNotNull { requestId ->
                val ids = Fence.parseRequestId(requestId) ?: return@mapNotNull null
                val fence = fencesById[ids.second]
                val state = storage.deviceStateRepository.getState(ids.second)?.state
                MonitoredGeofenceInfo(
                    geofenceId = ids.second,
                    clusterId = ids.first,
                    title = fence?.title,
                    latitude = fence?.latitude ?: 0.0,
                    longitude = fence?.longitude ?: 0.0,
                    radiusM = fence?.radiusM ?: 0.0,
                    state = state?.wireValue ?: "unknown"
                )
            }
            .sortedBy { it.geofenceId }
    }

    /** Son tetiklenen geçişler (en yeniden eskiye). */
    fun recentTriggeredEvents(limit: Int): List<TriggeredEventInfo> =
        storage.triggerHistoryRepository.recent(limit).map {
            TriggeredEventInfo(
                geofenceId = it.geofenceId,
                clusterId = it.clusterId,
                title = it.title,
                eventType = it.eventType.wireValue,
                occurredAtMillis = it.occurredAtMillis,
                campaignIds = it.campaignIds,
                accuracyM = it.accuracyM,
                stateOnly = it.stateOnly,
                syntheticTransition = it.syntheticTransition
            )
        }

    // ---- core reeval ----

    /**
     * [fireCampaigns] yalnızca movement kaynaklı reeval'de true. Diğer (start / organic sync /
     * silent push / active-window) reeval'lerde sentetik geçiş state-only işlenir, kampanya atmaz.
     */
    private suspend fun reeval(
        location: Location?,
        syncAllowed: Boolean,
        force: Boolean,
        fireCampaigns: Boolean = false
    ) {
        if (location != null) lastReevalLocation = location

        // force=true kanalları (start / forceResync / silent push) tamir kanalıdır: store OS'un
        // gerçeğinden sapmış olabilir → FULL (remove-all + re-register). Diğer yollar DIFF:
        // değişmeyen fence'e dokunulmaz, OS dwell loiter timer'ı korunur (doc 23 İş 1).
        val registerMode = if (force) {
            OsGeofenceRegistrar.RegisterMode.FULL
        } else {
            OsGeofenceRegistrar.RegisterMode.DIFF
        }

        if (syncAllowed) {
            when (syncer.sync(location?.latitude, location?.longitude)) {
                is GeofenceSyncer.SyncResult.NoSubscription ->
                    DengageLogger.debug("GeofenceEngine -> no subscription, skip register")
                else -> Unit
            }
        } else {
            DengageLogger.debug("GeofenceEngine -> transit mode: local top-N only, server sync skipped")
        }

        // Storage'daki güncel fence'lerden top-N seç + OS register (cache'ten, transit modunda bile)
        if (location != null) {
            registerTopN(location, registerMode)
            reconcileContainment(location, fireCampaigns)
        }
        if (force || location != null) {
            location?.let { heartbeatSender.maybeSend(it, remoteConfig.config().heartbeatIntervalMinutes, force = force) }
        }
    }

    /**
     * Synthetic transition check (doc 22 §2.1). Without waiting for the OS callback, closes the
     * gaps between the location and the state table with our own events (missed exit, enter that
     * never arrived).
     *
     * [fireCampaigns]: movement kaynaklı reeval'de true → her tip kampanya tetikleyebilir. Pasif
     * wake'lerde (silent push / sync-only / cross-fence) yalnız ENTER bastırılır:
     * - EXIT/DWELL muğlaklıksız gerçek geçiştir: sentetik EXIT yalnız state INSIDE iken üretilir
     *   (daha önce gözlemlenmiş bir enter var), DWELL koşulu ise şu an doğrudur (kullanıcı hâlâ
     *   içeride ve süre dolmuş) → occurredAt=now dürüst; bastırmak kampanyayı kalıcı kaybettirebilir
     *   (OS exit'i hiç gelmeyebilir — ör. fence top-N'den düşüp OS'tan silindiyse).
     * - ENTER'da "initial containment" muğlaklığı var (gece sync'lenen ev fence'i: cihaz zaten
     *   içerideydi, geçiş yok) → state-only kalır; pending-campaign borcu + INITIAL_TRIGGER_ENTER
     *   gerçek geçişi kapatır (doc 23 İş 3).
     */
    private suspend fun reconcileContainment(location: Location, fireCampaigns: Boolean) {
        val pending = containmentReconciler.reconcile(location)
        if (pending.isEmpty()) return
        // One call per event type: `handle` flushes the queue internally.
        pending.groupBy({ it.second }, { it.first.requestId })
            .forEach { (eventType, requestIds) ->
                val fire = fireCampaigns || eventType != GeofenceEventType.ENTER
                triggerHandler.handle(
                    eventType, requestIds, location,
                    fireCampaigns = fire,
                    syntheticTransition = true
                )
            }
    }

    private fun registerTopN(location: Location, mode: OsGeofenceRegistrar.RegisterMode) {
        val config = remoteConfig.config()
        val all = storage.fenceRepository.loadAll()
        val selected = topNSelector.select(all, location.latitude, location.longitude, config.topN)
        registrar.register(selected, mode)
        activeWindowScheduler.schedule(selected)
        DengageLogger.debug("GeofenceEngine -> registered ${selected.size}/${all.size} fences (topN=${config.topN}, mode=$mode)")
    }

    // ---- helpers ----

    private fun hasLocationPermission(): Boolean =
        GeofencePermissionsHelper.fineLocationPermission(context) ||
            GeofencePermissionsHelper.coarseLocationPermission(context)

    private fun currentLocation(): Location? {
        val now = System.currentTimeMillis()
        val cached = lastReevalLocation
        // Cache tazeyse yeterli (movement reeval'leri zaten taze fix'le gelir).
        if (cached != null && now - cached.time <= FRESH_LOCATION_MAX_AGE_MS) return cached

        // Bayat cache ile reconcile edilmez: reconciler 15 dk üstü fix'i zaten reddediyor, yani
        // taze fix alınmazsa foreground/organic reeval hiçbir sentetik tamir yapamaz (doc 23 İş 4 —
        // "içerideyim ama SDK bilmiyor" gecikmesinin kök nedeni). Organic/foreground anları
        // kullanıcı-görünür; tek atımlık BALANCED fix maliyeti kabul edilebilir.
        val client = LocationServices.getFusedLocationProviderClient(context)
        return try {
            Tasks.await(
                client.getCurrentLocation(
                    Priority.PRIORITY_BALANCED_POWER_ACCURACY,
                    CancellationTokenSource().token
                ),
                10, TimeUnit.SECONDS
            ) ?: cached ?: Tasks.await(client.lastLocation, 5, TimeUnit.SECONDS)
        } catch (e: Exception) {
            DengageLogger.error("GeofenceEngine -> fresh location failed: ${e.message}")
            cached ?: try {
                Tasks.await(client.lastLocation, 5, TimeUnit.SECONDS)
            } catch (e2: Exception) {
                DengageLogger.error("GeofenceEngine -> lastLocation failed: ${e2.message}")
                null
            }
        }
    }

    private fun scheduleResume(minutes: Long) {
        val work = OneTimeWorkRequestBuilder<ResumeSlcWorker>()
            .setInitialDelay(minutes, TimeUnit.MINUTES)
            .addTag(RESUME_WORK_NAME)
            .build()
        WorkManager.getInstance(context).enqueueUniqueWork(
            RESUME_WORK_NAME, ExistingWorkPolicy.REPLACE, work
        )
    }

    companion object {
        private const val RESUME_WORK_NAME = "dengage_geofence_resume"

        /** Bu yaştan taze cache/fix "güncel konum" sayılır; üstünde tek atımlık taze fix istenir. */
        private const val FRESH_LOCATION_MAX_AGE_MS = 2L * 60_000L

        /** Foreground kaynaklı organik sync'ler arası asgari süre (İş 5 debounce). */
        private const val FOREGROUND_SYNC_DEBOUNCE_MS = 60_000L
    }
}
