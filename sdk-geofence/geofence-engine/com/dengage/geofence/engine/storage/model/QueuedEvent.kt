package com.dengage.geofence.engine.storage.model

import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventType

/**
 * Offline durumda kuyruklanan, online olunca `POST /event-signal` v2 ile flush edilen trigger.
 * [idempotencyKey] dedup için server tarafında kullanılır (contract §3).
 */
data class QueuedEvent(
    val idempotencyKey: String,
    val geofenceId: Int,
    val clusterId: Int,
    val campaignId: Int?,
    val eventType: GeofenceEventType,
    val latitude: Double,
    val longitude: Double,
    val occurredAtMillis: Long
)
