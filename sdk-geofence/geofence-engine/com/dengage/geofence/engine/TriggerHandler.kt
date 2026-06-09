package com.dengage.geofence.engine

import android.content.Context
import android.location.Location
import android.net.ConnectivityManager
import android.net.NetworkCapabilities
import android.os.Build
import com.dengage.geofence.engine.storage.DeviceStateRepository
import com.dengage.geofence.engine.storage.EventQueueRepository
import com.dengage.geofence.engine.storage.FenceRepository
import com.dengage.geofence.engine.storage.model.Fence
import com.dengage.geofence.engine.storage.model.FenceState
import com.dengage.geofence.engine.storage.model.QueuedEvent
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventType
import com.dengage.sdk.domain.geofence.model.sync.GeofenceTriggerType
import com.dengage.sdk.domain.geofence.model.sync.SyncCampaign
import com.dengage.sdk.util.DengageLogger
import com.google.android.gms.location.Geofence
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

    suspend fun handle(transitionType: Int, requestIds: List<String>, location: Location?) {
        val eventType = transitionToEventType(transitionType) ?: run {
            DengageLogger.debug("TriggerHandler -> unsupported transition $transitionType")
            return
        }
        val online = isOnline()
        val now = System.currentTimeMillis()

        for (requestId in requestIds) {
            val ids = Fence.parseRequestId(requestId) ?: continue
            val fence = fenceRepository.findById(ids.second) ?: continue

            updateDeviceState(fence, eventType, now)

            val matchingTrigger = eventTypeToTrigger(eventType)
            val matchingCampaigns = fence.campaigns.filter { it.triggerType == matchingTrigger }
            if (matchingCampaigns.isEmpty()) {
                DengageLogger.debug("TriggerHandler -> no ${matchingTrigger.wireValue} campaign for fence ${fence.fenceId}")
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
            geofenceId = fence.fenceId,
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
                notificationFirer.fire(it, fence.fenceId, campaign.campaignId)
            }
            eventQueue.enqueue(event, configProvider())
            DengageLogger.debug("TriggerHandler -> offline trigger queued ${event.idempotencyKey}")
        }
    }

    private fun updateDeviceState(fence: Fence, eventType: GeofenceEventType, now: Long) {
        when (eventType) {
            GeofenceEventType.ENTER -> deviceStateRepository.setState(
                fence.fenceId, fence.clusterId, FenceState.INSIDE,
                enteredAt = now, lastSeenAt = now, exitedAt = null
            )
            GeofenceEventType.DWELL -> {
                val existing = deviceStateRepository.getState(fence.fenceId)
                deviceStateRepository.setState(
                    fence.fenceId, fence.clusterId, FenceState.INSIDE,
                    enteredAt = existing?.enteredAt ?: now, lastSeenAt = now, exitedAt = null
                )
            }
            GeofenceEventType.EXIT -> {
                val existing = deviceStateRepository.getState(fence.fenceId)
                deviceStateRepository.setState(
                    fence.fenceId, fence.clusterId, FenceState.OUTSIDE,
                    enteredAt = existing?.enteredAt, lastSeenAt = now, exitedAt = now
                )
            }
        }
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
