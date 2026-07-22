package com.dengage.geofenceengine

import com.dengage.geofenceengine.storage.EventQueueRepository
import com.dengage.geofenceengine.storage.model.QueuedEvent
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.geofence.GeofenceRepository
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventSignalRequestV2
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventSource
import com.dengage.sdk.util.DengageLogger

/**
 * Offline kuyruktaki event'leri online olunca server'a flush eder (contract §3, doc 21 §6.5).
 * Başarılı gönderim sonrası kuyruktan ack'lenir; `source = replay` ile gönderilir.
 */
class EventQueueFlusher(
    private val eventQueue: EventQueueRepository,
    private val apiRepository: GeofenceRepository = GeofenceRepository()
) {

    suspend fun flush(batchSize: Int) {
        val subscription = Prefs.subscription ?: return
        val integrationKey = subscription.integrationKey
        if (integrationKey.isBlank()) return

        val batch = eventQueue.dequeueBatch(batchSize)
        if (batch.isEmpty()) return

        // Geçersiz (geofenceId <= 0) event'leri gönderme; kuyruktan temizle.
        // (Eski geofenceId alan uyumsuzluğundan kalan bayat replay event'leri buraya düşer.)
        val invalid = batch.filter { !it.isValid }
        if (invalid.isNotEmpty()) {
            eventQueue.ack(invalid.map { it.idempotencyKey })
            DengageLogger.debug("EventQueueFlusher -> purged ${invalid.size} invalid (geofenceId<=0) events")
        }
        val valid = batch.filter { it.isValid }
        if (valid.isEmpty()) return

        val acked = mutableListOf<String>()
        for (event in valid) {
            val sent = send(integrationKey, subscription.deviceId, subscription.contactKey, event, GeofenceEventSource.REPLAY)
            if (sent) acked.add(event.idempotencyKey) else break // network düştüyse dur, kalanı sonraki flush'a bırak
        }
        if (acked.isNotEmpty()) {
            eventQueue.ack(acked)
            DengageLogger.debug("EventQueueFlusher -> flushed ${acked.size} events")
        }
    }

    /** Online tek event gönderimi; başarısızsa kuyruğa bırakılması çağırana aittir. */
    suspend fun sendOnline(event: QueuedEvent): Boolean {
        // Geçersiz event kuyruğa bırakılmasın (true dön ki çağıran enqueue etmesin).
        if (!event.isValid) {
            DengageLogger.debug("EventQueueFlusher -> dropping invalid event (geofenceId<=0)")
            return true
        }
        val subscription = Prefs.subscription ?: return false
        val integrationKey = subscription.integrationKey
        if (integrationKey.isBlank()) return false
        return send(integrationKey, subscription.deviceId, subscription.contactKey, event, GeofenceEventSource.ONLINE)
    }

    private suspend fun send(
        integrationKey: String,
        deviceId: String?,
        contactKey: String?,
        event: QueuedEvent,
        source: GeofenceEventSource
    ): Boolean {
        return try {
            val request = GeofenceEventSignalRequestV2(
                deviceId = deviceId ?: "",
                contactKey = contactKey,
                geofenceId = event.geofenceId,
                clusterId = event.clusterId,
                campaignId = event.campaignId,
                eventType = event.eventType,
                latitude = event.latitude,
                longitude = event.longitude,
                accuracyM = event.accuracyM,
                occurredAt = GeofenceEventSignalRequestV2.iso(event.occurredAtMillis),
                ingestedAt = GeofenceEventSignalRequestV2.isoNow(),
                idempotencyKey = event.idempotencyKey,
                source = source
            )
            val response = apiRepository.sendGeofenceEventSignalV2(integrationKey, request)
            // 2xx ve 409 (idempotent no-op) başarılı sayılır (contract §6)
            val success = response.isSuccessful || response.code() == 409
            if (!success) {
                DengageLogger.error("EventQueueFlusher -> http ${response.code()}")
                GeofenceDebugLogger.error(
                    "Geofence event-signal send failed",
                    mapOf(
                        "httpCode" to response.code().toString(),
                        "geofenceId" to event.geofenceId.toString(),
                        "eventType" to event.eventType.wireValue,
                        "source" to source.wireValue
                    )
                )
            }
            success
        } catch (e: Exception) {
            DengageLogger.error("EventQueueFlusher -> send failed: ${e.message}")
            GeofenceDebugLogger.error(
                "Geofence event-signal send failed",
                mapOf(
                    "error" to (e.message ?: "unknown"),
                    "geofenceId" to event.geofenceId.toString(),
                    "eventType" to event.eventType.wireValue,
                    "source" to source.wireValue
                )
            )
            false
        }
    }
}
