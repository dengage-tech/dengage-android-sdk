package com.dengage.geofence.engine

import android.location.Location
import com.dengage.geofence.engine.storage.SyncMetadataRepository
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.geofence.GeofenceRepository
import com.dengage.sdk.domain.geofence.model.sync.DeviceHeartbeatRequest
import com.dengage.sdk.util.DengageLogger

/**
 * `POST /devices/heartbeat` — cihaz konumu raporlama (contract §2, K-device_last_location).
 * `heartbeatIntervalMinutes`'e göre throttle edilir.
 */
class HeartbeatSender(
    private val syncMetadata: SyncMetadataRepository,
    private val apiRepository: GeofenceRepository = GeofenceRepository()
) {

    /** Interval dolduysa heartbeat gönderir; [force] true ise interval'i yok sayar. */
    suspend fun maybeSend(location: Location, intervalMinutes: Int, force: Boolean = false) {
        val now = System.currentTimeMillis()
        val last = syncMetadata.lastHeartbeatAt ?: 0L
        if (!force && (now - last) < intervalMinutes * 60_000L) {
            return
        }

        val subscription = Prefs.subscription ?: return
        val integrationKey = subscription.integrationKey
        if (integrationKey.isBlank()) return

        try {
            val request = DeviceHeartbeatRequest(
                deviceId = subscription.deviceId ?: "",
                contactKey = subscription.contactKey,
                latitude = location.latitude,
                longitude = location.longitude,
                accuracyM = if (location.hasAccuracy()) location.accuracy else null,
                capturedAtMillis = if (location.time > 0) location.time else now
            )
            val response = apiRepository.sendDeviceHeartbeat(integrationKey, request)
            if (response.isSuccessful) {
                syncMetadata.lastHeartbeatAt = now
                DengageLogger.debug("HeartbeatSender -> sent heartbeat")
            } else {
                DengageLogger.error("HeartbeatSender -> http ${response.code()}")
            }
        } catch (e: Exception) {
            DengageLogger.error("HeartbeatSender -> error ${e.message}")
        }
    }
}
