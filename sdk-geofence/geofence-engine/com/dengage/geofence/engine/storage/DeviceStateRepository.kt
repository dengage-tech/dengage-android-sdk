package com.dengage.geofence.engine.storage

import com.dengage.geofence.engine.storage.model.DeviceFenceState
import com.dengage.geofence.engine.storage.model.FenceState

/** Cihaz başına fence dwell/state tracking (doc 21 §6.2). */
interface DeviceStateRepository {
    fun setState(
        geofenceId: Int,
        clusterId: Int,
        state: FenceState,
        enteredAt: Long?,
        lastSeenAt: Long?,
        exitedAt: Long?
    )

    fun getState(geofenceId: Int): DeviceFenceState?
    fun clear()
}
