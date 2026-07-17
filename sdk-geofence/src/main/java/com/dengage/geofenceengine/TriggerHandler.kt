package com.dengage.geofenceengine

import android.content.Context
import android.location.Location
import android.net.ConnectivityManager
import android.net.NetworkCapabilities
import android.os.Build
import com.dengage.geofenceengine.storage.DeviceStateRepository
import com.dengage.geofenceengine.storage.EventQueueRepository
import com.dengage.geofenceengine.storage.FenceRepository
import com.dengage.geofenceengine.storage.model.Fence
import com.dengage.geofenceengine.storage.model.FenceState
import com.dengage.geofenceengine.storage.model.QueuedEvent
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
        handle(eventType, requestIds, location)
    }

    /**
     * Event-type based entry point. Besides the OS callback ([handleTransition]), synthetic
     * transitions (doc 22 §2.1, [ContainmentReconciler]) also come through here — dedup, campaign
     * matching and event-signal delivery are identical for both.
     */
    suspend fun handle(eventType: GeofenceEventType, requestIds: List<String>, location: Location?) {
        val online = isOnline()
        val now = System.currentTimeMillis()

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

            // Davranış paritesi: enter'da host interceptor'ı tetikle (v1 ile aynı hook)
            if (eventType == GeofenceEventType.ENTER) {
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
            if (matchingCampaigns.isEmpty()) {
                DengageLogger.debug("TriggerHandler -> no ${matchingTrigger.wireValue} campaign for fence ${fence.geofenceId}")
                continue
            }

            val lat = location?.latitude ?: fence.latitude
            val lon = location?.longitude ?: fence.longitude

            for (campaign in matchingCampaigns) {
                dispatch(fence, campaign, eventType, lat, lon, now, online)
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
            occurredAtMillis = occurredAt
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
}
