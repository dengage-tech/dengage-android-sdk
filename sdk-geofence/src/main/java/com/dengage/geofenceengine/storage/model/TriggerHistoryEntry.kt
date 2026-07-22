package com.dengage.geofenceengine.storage.model

import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventType

/**
 * Tetiklenen bir transition'ın geçmiş kaydı (teşhis/QA).
 * Dedup'tan geçmiş, yani *gerçekten* olmuş geçişleri temsil eder.
 */
data class TriggerHistoryEntry(
    val geofenceId: Int,
    val clusterId: Int,
    val title: String?,
    val eventType: GeofenceEventType,
    val occurredAtMillis: Long,
    /** Bu geçişle eşleşen kampanyalar; boşsa geçiş oldu ama kampanya eşleşmedi. */
    val campaignIds: List<Int>,
    /** Geçiş anındaki yatay konum doğruluğu (metre); yoksa null. */
    val accuracyM: Double? = null,
    /** true → state-only reconcile (silent/sync-only reeval): state güncellendi ama kampanya atılmadı. */
    val stateOnly: Boolean = false
)
