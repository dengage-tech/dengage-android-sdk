package com.dengage.geofence.engine

import android.annotation.SuppressLint
import android.content.Context
import android.location.Location
import androidx.work.ExistingWorkPolicy
import androidx.work.OneTimeWorkRequestBuilder
import androidx.work.WorkManager
import com.dengage.geofence.engine.storage.GeofenceStorage
import com.dengage.geofence.engine.storage.model.Fence
import com.dengage.geofence.engine.worker.ResumeSlcWorker
import com.dengage.geofence.manager.GeofencePermissionsHelper
import com.dengage.sdk.util.DengageLogger
import com.google.android.gms.location.LocationServices
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
        configProvider = { remoteConfig.config().offlineQueueMaxSize }
    )

    private val adaptiveThreshold = AdaptiveThresholdCalculator(
        configProvider = { remoteConfig.config().adaptiveThreshold }
    )
    private val wakeupCap = WakeupCapController(
        configProvider = { remoteConfig.config().wakeupCap },
        onPause = { movementListener.stop() },
        onResume = { movementListener.start(remoteConfig.config().reevaluationDistanceMeters) },
        scheduleResume = { minutes -> scheduleResume(minutes) }
    )

    @Volatile
    private var lastReevalLocation: Location? = null

    @Volatile
    private var running = false

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
        movementListener.start(remoteConfig.config().reevaluationDistanceMeters)
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

    fun requestOrganicSync(reason: OrganicSyncTrigger.Reason) {
        if (!remoteConfig.geofenceEnabled()) return
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

    fun handleMovement(location: Location) {
        if (!remoteConfig.geofenceEnabled()) {
            stop(); return
        }
        when (wakeupCap.recordWakeup()) {
            WakeupAction.SKIP_PAUSED -> return
            WakeupAction.PAUSE_AND_SKIP -> return
            WakeupAction.PROCEED -> Unit
        }

        scope.launch {
            heartbeatSender.maybeSend(location, remoteConfig.config().heartbeatIntervalMinutes)

            when (val decision = adaptiveThreshold.shouldReeval(location, lastReevalLocation)) {
                is ReevalDecision.Reeval -> reeval(location, syncAllowed = decision.syncAllowed, force = false)
                is ReevalDecision.Skip -> DengageLogger.debug("GeofenceEngine -> reeval skipped (${decision.reason})")
            }
        }
    }

    // ---- trigger ----

    fun handleGeofenceTransition(transitionType: Int, requestIds: List<String>, location: Location?) {
        // Region monitoring callback'i pause süresini etkilemez; ayrıca resume fırsatı verir (K13)
        wakeupCap.attemptResume()
        scope.launch {
            triggerHandler.handle(transitionType, requestIds, location)
        }
    }

    // ---- core reeval ----

    private suspend fun reeval(location: Location?, syncAllowed: Boolean, force: Boolean) {
        if (location != null) lastReevalLocation = location

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
            registerTopN(location)
        }
        if (force || location != null) {
            location?.let { heartbeatSender.maybeSend(it, remoteConfig.config().heartbeatIntervalMinutes, force = force) }
        }
    }

    private fun registerTopN(location: Location) {
        val config = remoteConfig.config()
        val all = storage.fenceRepository.loadAll()
        val selected = topNSelector.select(all, location.latitude, location.longitude, config.topN)
        registrar.register(selected)
        activeWindowScheduler.schedule(selected)
        DengageLogger.debug("GeofenceEngine -> registered ${selected.size}/${all.size} fences (topN=${config.topN})")
    }

    // ---- helpers ----

    private fun hasLocationPermission(): Boolean =
        GeofencePermissionsHelper.fineLocationPermission(context) ||
            GeofencePermissionsHelper.coarseLocationPermission(context)

    private fun currentLocation(): Location? {
        lastReevalLocation?.let { return it }
        return try {
            val client = LocationServices.getFusedLocationProviderClient(context)
            Tasks.await(client.lastLocation, 5, TimeUnit.SECONDS)
        } catch (e: Exception) {
            DengageLogger.error("GeofenceEngine -> lastLocation failed: ${e.message}")
            null
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
    }
}
