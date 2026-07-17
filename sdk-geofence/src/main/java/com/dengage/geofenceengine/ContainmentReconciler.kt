package com.dengage.geofenceengine

import android.location.Location
import com.dengage.geofenceengine.storage.DeviceStateRepository
import com.dengage.geofenceengine.storage.FenceRepository
import com.dengage.geofenceengine.storage.model.Fence
import com.dengage.geofenceengine.storage.model.FenceState
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventType
import com.dengage.sdk.util.DengageLogger

/**
 * Synthetic transitions — local containment check on every wake (doc 22 §2.1).
 *
 * Instead of waiting for the OS geofence callback, the device's current location is compared
 * against the stored fences, and a transition is produced wherever the state table disagrees.
 * What this buys us:
 * - **Missed EXIT repair:** if the OS never delivers an exit, the state stays INSIDE forever and
 *   [TriggerHandler]'s dedup swallows every subsequent enter. Only a comparison against the real
 *   location can detect this.
 * - **Events the OS never produces:** with a small radius at high speed the OS may not sample
 *   inside the fence at all; we still catch it if the location is inside at wake time.
 * - **Doze batching:** if the OS holds an event back for hours, we get there first.
 *
 * Produced transitions go through the normal [TriggerHandler.handle] path: `occurredAt = now`
 * (genuinely fresh) and the existing `source` semantics are preserved (online, or replay when
 * offline). If one races an OS callback, `isDuplicateTransition` swallows the loser.
 */
class ContainmentReconciler(
    private val fenceRepository: FenceRepository,
    private val deviceStateRepository: DeviceStateRepository
) {

    /**
     * Compares the location against the state table and returns the transitions that should fire.
     * Pure computation — it neither sends events nor writes state; both are [TriggerHandler]'s job.
     */
    fun reconcile(location: Location): List<Pair<Fence, GeofenceEventType>> {
        val pending = mutableListOf<Pair<Fence, GeofenceEventType>>()

        for (fence in fenceRepository.loadAll()) {
            if (fence.geofenceId <= 0) continue

            val distance = distanceMeters(location, fence)
            val state = deviceStateRepository.getState(fence.geofenceId)?.state
            val deviceThinksInside = isInsideState(state)

            if (distance <= fence.radiusM) {
                // Actually inside but the table does not know — the OS enter is late or never came.
                if (!deviceThinksInside) {
                    pending += fence to GeofenceEventType.ENTER
                }
            } else if (deviceThinksInside && distance > fence.radiusM * EXIT_HYSTERESIS_FACTOR) {
                // The table says inside, yet we are outside even allowing for hysteresis — missed exit.
                pending += fence to GeofenceEventType.EXIT
            }
        }

        if (pending.isNotEmpty()) {
            DengageLogger.debug("ContainmentReconciler -> ${pending.size} synthetic transition(s)")
        }
        return pending
    }

    /** DWELL_PENDING also means the device is inside (inside + dwell already fired). */
    private fun isInsideState(state: FenceState?): Boolean =
        state == FenceState.INSIDE || state == FenceState.DWELL_PENDING

    private fun distanceMeters(location: Location, fence: Fence): Double {
        val result = FloatArray(1)
        Location.distanceBetween(
            location.latitude, location.longitude,
            fence.latitude, fence.longitude,
            result
        )
        return result[0].toDouble()
    }

    companion object {
        /**
         * Exit hysteresis: no exit is produced until the device is beyond `radius × 1.5`,
         * so that location error near the boundary cannot cause enter/exit flapping.
         */
        private const val EXIT_HYSTERESIS_FACTOR = 1.5
    }
}
