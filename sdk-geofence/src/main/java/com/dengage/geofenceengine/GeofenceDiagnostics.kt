package com.dengage.geofenceengine

/** OS'a register edilmiş bir fence'in teşhis görünümü. */
data class MonitoredGeofenceInfo(
    val geofenceId: Int,
    val clusterId: Int,
    val title: String?,
    val latitude: Double,
    val longitude: Double,
    val radiusM: Double,
    /** `inside` / `outside` / `dwell_pending` / `unknown` */
    val state: String
)

/** Tetiklenmiş bir geçişin teşhis görünümü. */
data class TriggeredEventInfo(
    val geofenceId: Int,
    val clusterId: Int,
    val title: String?,
    /** `enter` / `exit` / `dwell` */
    val eventType: String,
    val occurredAtMillis: Long,
    /** Bu geçişle eşleşen kampanyalar; boşsa geçiş oldu ama kampanya eşleşmedi. */
    val campaignIds: List<Int>,
    /** Geçiş anındaki yatay konum doğruluğu (metre); yoksa null. */
    val accuracyM: Double?,
    /** true → state-only reconcile (silent/sync-only reeval): state güncellendi ama kampanya atılmadı. */
    val stateOnly: Boolean
)
