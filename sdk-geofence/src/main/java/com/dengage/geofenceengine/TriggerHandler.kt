package com.dengage.geofenceengine

import android.content.Context
import android.location.Location
import android.net.ConnectivityManager
import android.net.NetworkCapabilities
import android.os.Build
import com.dengage.geofenceengine.storage.DeviceStateRepository
import com.dengage.geofenceengine.storage.EventQueueRepository
import com.dengage.geofenceengine.storage.FenceRepository
import com.dengage.geofenceengine.storage.TriggerHistoryRepository
import com.dengage.geofenceengine.storage.model.Fence
import com.dengage.geofenceengine.storage.model.FenceState
import com.dengage.geofenceengine.storage.model.QueuedEvent
import com.dengage.geofenceengine.storage.model.TriggerHistoryEntry
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventType
import com.dengage.sdk.domain.geofence.model.sync.GeofenceTriggerType
import com.dengage.sdk.domain.geofence.model.sync.SyncCampaign
import com.dengage.sdk.util.DengageLogger
import com.google.android.gms.location.Geofence
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock
import java.util.UUID

/**
 * OS geofence transition handler (doc 21 §6.5).
 * - Trigger type matcher: enter/exit/dwell → eşleşen kampanyalar gate'lenir.
 * - Online: event-signal v2 server'a gönderilir (server push'u orkestre eder).
 * - Offline: cache'lenen `offlinePushContent` ile local notification anında fire (K10) + event kuyruğa.
 */
class TriggerHandler(
    private val context: Context,
    private val fenceRepository: FenceRepository,
    private val deviceStateRepository: DeviceStateRepository,
    private val eventQueue: EventQueueRepository,
    private val notificationFirer: LocalNotificationFirer,
    private val eventFlusher: EventQueueFlusher,
    private val triggerHistory: TriggerHistoryRepository,
    private val configProvider: () -> Int // offlineQueueMaxSize
) {

    /** Guards the dedup's read-check-write section against concurrent `handle` calls. */
    private val stateMutex = Mutex()

    /** Maps the OS (GMS) transition constant to an event type and delegates to [handle]. */
    suspend fun handleTransition(transitionType: Int, requestIds: List<String>, location: Location?) {
        val eventType = transitionToEventType(transitionType) ?: run {
            DengageLogger.debug("TriggerHandler -> unsupported transition $transitionType")
            return
        }
        // OS transition: occurredAt is derived from the triggering fix time (past time if the OS held
        // the event back), so the server can tell it is stale (doc 22 §3.1).
        handle(eventType, requestIds, location, occurredAtMillis(location))
    }

    /**
     * Event-type based entry point. Besides the OS callback ([handleTransition]), synthetic
     * transitions (doc 22 §2.1, [ContainmentReconciler]) also come through here — dedup, campaign
     * matching and event-signal delivery are identical for both.
     *
     * [occurredAtMillis] null → processing time. OS callbacks pass the fix time; synthetic/dwell
     * paths leave it null (they really happened "now").
     *
     * [fireCampaigns] false → state is updated + recorded in history, but interceptor and event-signal
     * are skipped (state-only). A synthetic transition only fires campaigns when it comes from a
     * movement/OS wake; silent push / sync-only reeval stays silent.
     */
    suspend fun handle(
        eventType: GeofenceEventType,
        requestIds: List<String>,
        location: Location?,
        occurredAtMillis: Long? = null,
        fireCampaigns: Boolean = true
    ) {
        val online = isOnline()
        val now = System.currentTimeMillis()
        // occurredAt: when the transition happened (fix time). createdAt/state/dedup use `now`.
        val occurred = occurredAtMillis ?: now

        for (requestId in requestIds) {
            val ids = Fence.parseRequestId(requestId) ?: continue
            val fence = fenceRepository.findById(ids.second) ?: continue
            if (fence.geofenceId <= 0) {
                DengageLogger.debug("TriggerHandler -> skipping invalid fence (geofenceId<=0)")
                continue
            }

            // Edge-detection / dedup: the OS can deliver several callbacks for the same physical
            // transition (initialTrigger + normal ENTER, after a re-register, ...), and a synthetic
            // transition can race an OS callback. Only fire on a real state change.
            //
            // Keep read-check-write atomic with a mutex: `handle` can be called concurrently from
            // separate coroutines (OS transition + reconciler); otherwise both read OUTSIDE, both
            // fire, and the dedup is defeated.
            val proceed = stateMutex.withLock {
                val previous = deviceStateRepository.getState(fence.geofenceId)
                if (isDuplicateTransition(eventType, previous?.state)) {
                    DengageLogger.debug("TriggerHandler -> duplicate ${eventType.wireValue} for fence ${fence.geofenceId}, skipping")
                    false
                } else {
                    updateDeviceState(fence, eventType, now)
                    true
                }
            }
            if (!proceed) continue

            // Davranış paritesi: enter'da host interceptor'ı tetikle (v1 ile aynı hook).
            // state-only (fireCampaigns=false) modda interceptor da atlanır.
            if (eventType == GeofenceEventType.ENTER && fireCampaigns) {
                try {
                    com.dengage.geofence.DengageGeofence.geofenceInterceptor?.onGeofenceEnter(
                        latitude = fence.latitude,
                        longitude = fence.longitude,
                        radius = fence.radiusM,
                        clusterId = fence.clusterId,
                        clusterName = null,
                        geofenceItemId = fence.geofenceId,
                        geofenceItemName = fence.title
                    )
                } catch (e: Exception) {
                    DengageLogger.error("TriggerHandler -> interceptor threw: ${e.message}")
                }
            }

            val matchingTrigger = eventTypeToTrigger(eventType)
            val matchingCampaigns = fence.campaigns.filter { it.triggerType == matchingTrigger }
            // Yatay doğruluk (metre); yoksa null (heartbeat ile aynı kural).
            val accuracyM = location?.takeIf { it.hasAccuracy() }?.accuracy?.toDouble()

            // Diagnostics history: the transition passed dedup, so it really happened. Recorded even
            // when no campaign matches, so "transition happened but no campaign" can be told apart
            // from "transition never happened".
            triggerHistory.record(
                TriggerHistoryEntry(
                    geofenceId = fence.geofenceId,
                    clusterId = fence.clusterId,
                    title = fence.title,
                    eventType = eventType,
                    occurredAtMillis = occurred,
                    campaignIds = matchingCampaigns.map { it.campaignId },
                    accuracyM = accuracyM,
                    stateOnly = !fireCampaigns
                ),
                TRIGGER_HISTORY_MAX_SIZE
            )

            if (matchingCampaigns.isEmpty()) {
                DengageLogger.debug("TriggerHandler -> no ${matchingTrigger.wireValue} campaign for fence ${fence.geofenceId}")
                continue
            }

            // state-only: state güncellendi + geçmişe yazıldı; kampanya (interceptor + event-signal) atlanır.
            if (!fireCampaigns) {
                DengageLogger.debug("TriggerHandler -> state-only reconcile for fence ${fence.geofenceId}, campaigns suppressed")
                continue
            }

            val lat = location?.latitude ?: fence.latitude
            val lon = location?.longitude ?: fence.longitude

            for (campaign in matchingCampaigns) {
                dispatch(fence, campaign, eventType, lat, lon, accuracyM, occurred, online)
            }
        }

        // Online'a yeni geçtiysek kuyrukta bekleyen offline event'leri de flush et
        if (online) {
            eventFlusher.flush(configProvider())
        }
    }

    private suspend fun dispatch(
        fence: Fence,
        campaign: SyncCampaign,
        eventType: GeofenceEventType,
        lat: Double,
        lon: Double,
        accuracyM: Double?,
        occurredAt: Long,
        online: Boolean
    ) {
        val event = QueuedEvent(
            idempotencyKey = UUID.randomUUID().toString(),
            geofenceId = fence.geofenceId,
            clusterId = fence.clusterId,
            campaignId = campaign.campaignId,
            eventType = eventType,
            latitude = lat,
            longitude = lon,
            occurredAtMillis = occurredAt,
            accuracyM = accuracyM
        )

        if (online) {
            val sent = eventFlusher.sendOnline(event)
            if (!sent) {
                DengageLogger.debug("TriggerHandler -> online send failed, queueing ${event.idempotencyKey}")
                eventQueue.enqueue(event, configProvider())
            }
        } else {
            // Offline: local notification anında (K10) + event kuyruğa (online olunca flush)
            campaign.offlinePushContent?.let {
                notificationFirer.fire(it, fence.geofenceId, campaign.campaignId)
            }
            eventQueue.enqueue(event, configProvider())
            DengageLogger.debug("TriggerHandler -> offline trigger queued ${event.idempotencyKey}")
        }
    }

    private fun updateDeviceState(fence: Fence, eventType: GeofenceEventType, now: Long) {
        when (eventType) {
            GeofenceEventType.ENTER -> deviceStateRepository.setState(
                fence.geofenceId, fence.clusterId, FenceState.INSIDE,
                enteredAt = now, lastSeenAt = now, exitedAt = null
            )
            GeofenceEventType.DWELL -> {
                // Dwell atıldı → DWELL_PENDING = "inside ve bu ziyarette dwell zaten fire edildi".
                // Sonraki dwell callback'leri (OS tekrarı / re-register sonrası loitering) böylece dedup edilir.
                val existing = deviceStateRepository.getState(fence.geofenceId)
                deviceStateRepository.setState(
                    fence.geofenceId, fence.clusterId, FenceState.DWELL_PENDING,
                    enteredAt = existing?.enteredAt ?: now, lastSeenAt = now, exitedAt = null
                )
            }
            GeofenceEventType.EXIT -> {
                val existing = deviceStateRepository.getState(fence.geofenceId)
                deviceStateRepository.setState(
                    fence.geofenceId, fence.clusterId, FenceState.OUTSIDE,
                    enteredAt = existing?.enteredAt, lastSeenAt = now, exitedAt = now
                )
            }
        }
    }

    /**
     * Aynı geçiş için tekrarlanan OS callback'lerini ele; her tetik tipi ziyaret başına en fazla bir kez fire eder.
     * State makinesi: enter→INSIDE, dwell→DWELL_PENDING (dwell atıldı), exit→OUTSIDE.
     * - enter: cihaz zaten fence içindeyse (INSIDE veya DWELL_PENDING) yinelenmedir.
     * - dwell: yalnızca taze INSIDE iken bir kez; DWELL_PENDING (zaten atıldı) veya OUTSIDE/null (bayat) → yinelenmedir.
     * - exit: cihaz zaten dışarıdaysa (OUTSIDE/null) yinelenmedir.
     */
    private fun isDuplicateTransition(eventType: GeofenceEventType, previous: FenceState?): Boolean =
        when (eventType) {
            GeofenceEventType.ENTER -> previous == FenceState.INSIDE || previous == FenceState.DWELL_PENDING
            GeofenceEventType.EXIT -> previous == null || previous == FenceState.OUTSIDE
            GeofenceEventType.DWELL -> previous != FenceState.INSIDE
        }

    /**
     * Derives occurredAt (epoch millis) from the transition's fix time. If the OS held the event
     * back, the fix time is older than processing time, so the server can tell it is stale (doc 22 §3.1).
     * `Location.time` is the fix's wall-clock time. Falls back to processing time when it is missing
     * or implausible (future / absurdly old = bad clock).
     */
    private fun occurredAtMillis(location: Location?): Long {
        val now = System.currentTimeMillis()
        val fix = location?.time ?: return now
        if (fix <= 0L) return now
        if (fix > now + MAX_FUTURE_SKEW_MS) return now
        if (fix < now - MAX_FIX_AGE_MS) return now
        return fix
    }

    private fun transitionToEventType(transition: Int): GeofenceEventType? = when (transition) {
        Geofence.GEOFENCE_TRANSITION_ENTER -> GeofenceEventType.ENTER
        Geofence.GEOFENCE_TRANSITION_EXIT -> GeofenceEventType.EXIT
        Geofence.GEOFENCE_TRANSITION_DWELL -> GeofenceEventType.DWELL
        else -> null
    }

    private fun eventTypeToTrigger(eventType: GeofenceEventType): GeofenceTriggerType = when (eventType) {
        GeofenceEventType.ENTER -> GeofenceTriggerType.ENTER
        GeofenceEventType.EXIT -> GeofenceTriggerType.EXIT
        GeofenceEventType.DWELL -> GeofenceTriggerType.DWELL
    }

    @Suppress("DEPRECATION")
    private fun isOnline(): Boolean {
        return try {
            val cm = context.getSystemService(Context.CONNECTIVITY_SERVICE) as ConnectivityManager
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                val network = cm.activeNetwork ?: return false
                val caps = cm.getNetworkCapabilities(network) ?: return false
                caps.hasCapability(NetworkCapabilities.NET_CAPABILITY_INTERNET)
            } else {
                cm.activeNetworkInfo?.isConnected == true
            }
        } catch (e: Exception) {
            false
        }
    }

    companion object {
        /** Teşhis geçmişinde tutulan azami kayıt sayısı. */
        private const val TRIGGER_HISTORY_MAX_SIZE = 50

        /** Saat kayması toleransı: fix zamanı bu kadar gelecekteyse yok say. */
        private const val MAX_FUTURE_SKEW_MS = 60_000L

        /** Fix zamanı bu kadar eskiyse bozuk kabul edip işlenme anına düş (Doze ~4 saati kapsar). */
        private const val MAX_FIX_AGE_MS = 48L * 60L * 60L * 1000L
    }
}
