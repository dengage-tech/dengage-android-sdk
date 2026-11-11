package com.dengage.geofenceengine

import android.location.Location
import com.dengage.geofenceengine.storage.SyncMetadataRepository
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.geofence.GeofenceRepository
import com.dengage.sdk.domain.geofence.model.sync.DeviceHeartbeatRequest
import com.dengage.sdk.util.DengageLogger
import kotlinx.coroutines.sync.Mutex
import kotlinx.coroutines.sync.withLock

/**
 * `POST /devices/heartbeat` — cihaz konumu raporlama (contract §2, K-device_last_location).
 * `heartbeatIntervalMinutes`'e göre throttle edilir.
 */
class HeartbeatSender(
    private val syncMetadata: SyncMetadataRepository,
    private val apiRepository: GeofenceRepository = GeofenceRepository()
) {

    /** Interval kontrolü + slot rezervasyonunu eşzamanlı `maybeSend` çağrılarına karşı korur. */
    private val mutex = Mutex()

    /**
     * Interval dolduysa heartbeat gönderir. [force] true ise interval yerine [MIN_FORCE_GAP_MS]
     * tabanı uygulanır: force kanalları (start / forceResync / silent push / PROVIDERS_CHANGED)
     * kümelenince ya da periyodik tetiklenince heartbeat hacmini çoğaltmasın.
     */
    suspend fun maybeSend(location: Location, intervalMinutes: Int, force: Boolean = false) {
        val subscription = Prefs.subscription ?: return
        val integrationKey = subscription.integrationKey
        if (integrationKey.isBlank()) return

        val now = System.currentTimeMillis()
        // Kontrol + rezervasyon atomik: movement + reeval, art arda gelen FLP broadcast'leri,
        // start + ilk konum fix'i aynı anda buraya düşebilir. lastHeartbeatAt yalnızca ağ çağrısından
        // sonra yazıldığında hepsi eski değeri okuyup gönderiyordu (aynı saniyede çift heartbeat).
        val previous = mutex.withLock {
            val last = syncMetadata.lastHeartbeatAt
            val minGapMs = if (force) MIN_FORCE_GAP_MS else intervalMinutes * 60_000L
            if (now - (last ?: 0L) < minGapMs) return
            syncMetadata.lastHeartbeatAt = now
            last
        }

        val sent = try {
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
                DengageLogger.debug("HeartbeatSender -> sent heartbeat")
                true
            } else {
                DengageLogger.error("HeartbeatSender -> http ${response.code()}")
                GeofenceDebugLogger.error("Geofence heartbeat failed", mapOf("httpCode" to response.code().toString()))
                false
            }
        } catch (e: Exception) {
            DengageLogger.error("HeartbeatSender -> error ${e.message}")
            GeofenceDebugLogger.error("Geofence heartbeat failed", mapOf("error" to (e.message ?: "unknown")))
            false
        }

        // Başarısız: rezervasyonu geri al ki sonraki tetikleyici tam interval beklemeden yeniden denesin.
        if (!sent) {
            mutex.withLock {
                if (syncMetadata.lastHeartbeatAt == now) syncMetadata.lastHeartbeatAt = previous
            }
        }
    }

    companion object {
        /** force=true heartbeat'leri arası asgari süre — contract §4'teki en kısa interval (15 dk). */
        private const val MIN_FORCE_GAP_MS = 15L * 60_000L
    }
}
