package com.dengage.geofenceengine.storage.model

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
    val occurredAtMillis: Long,
    /** Yatay konum doğruluğu (metre); yoksa null. */
    val accuracyM: Double? = null,
    /**
     * true → geçişi OS bildirmedi, SDK çıkarsadı ([ContainmentReconciler], doc 22 §2.1).
     * Kuyrukta da tutulur ki offline replay'de bayrak kaybolmasın.
     */
    val syntheticTransition: Boolean = false
) {
    /** geofenceId <= 0 (ör. eski alan uyumsuzluğundan kalan bayat event'ler) geçersiz sayılır. */
    val isValid: Boolean get() = geofenceId > 0
}
