package com.dengage.geofence.engine.storage.model

/** Cihazın bir fence'e göre durumu (doc 21 §1.2 device_geofence_state.state). */
enum class FenceState {
    INSIDE,
    OUTSIDE,
    DWELL_PENDING;

    val wireValue: String
        get() = when (this) {
            INSIDE -> "inside"
            OUTSIDE -> "outside"
            DWELL_PENDING -> "dwell_pending"
        }

    companion object {
        fun fromWire(value: String?): FenceState = when (value?.lowercase()) {
            "inside" -> INSIDE
            "dwell_pending" -> DWELL_PENDING
            else -> OUTSIDE
        }
    }
}

/** Cihaz başına fence state kaydı. */
data class DeviceFenceState(
    val fenceId: Int,
    val clusterId: Int,
    val state: FenceState,
    val enteredAt: Long?,
    val lastSeenAt: Long?,
    val exitedAt: Long?
)
