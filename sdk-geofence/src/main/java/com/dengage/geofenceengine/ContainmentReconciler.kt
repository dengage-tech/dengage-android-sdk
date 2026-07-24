package com.dengage.geofenceengine

import android.location.Location
import android.os.SystemClock
import com.dengage.geofenceengine.storage.DeviceStateRepository
import com.dengage.geofenceengine.storage.FenceRepository
import com.dengage.geofenceengine.storage.model.Fence
import com.dengage.geofenceengine.storage.model.FenceState
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventType
import com.dengage.sdk.domain.geofence.model.sync.GeofenceTriggerType
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
     *
     * Confidence has two dimensions, and both feed the same margin:
     * - **Spatial:** `Location.accuracy` is the fix's uncertainty radius.
     * - **Temporal:** a cached fix describes where the device *was*. Every second since then is
     *   distance the device may have covered unobserved, so the age is converted into extra
     *   uncertainty via [ASSUMED_SPEED_MPS] and added on top of the accuracy.
     *
     * The reconciler only acts outside that combined margin; the uncertain band is left to the OS
     * (which has its own buffer and multiple samples). This kills both accuracy-blind false
     * transitions from a coarse fix and staleness-blind ones from a cached fix — the latter matters
     * because the wake path feeds us `FusedLocationProviderClient.lastLocation`, which carries no
     * freshness guarantee at all after a long Doze window.
     *
     * [nowElapsedNanos] is injectable for tests. It is monotonic (`SystemClock`), unlike
     * `Location.getTime()`, so a system clock change cannot corrupt the age.
     */
    fun reconcile(
        location: Location,
        nowElapsedNanos: Long = SystemClock.elapsedRealtimeNanos(),
        nowMillis: Long = System.currentTimeMillis()
    ): List<Pair<Fence, GeofenceEventType>> {
        // Accuracy yok/geçersizse tüm konum belirsiz sayılır → hiçbir fence için karar verme.
        if (!location.hasAccuracy()) {
            DengageLogger.debug("ContainmentReconciler -> skipped: location accuracy unknown")
            return emptyList()
        }
        // A fix older than the cap is not worth reasoning about at any margin; leave it to the OS.
        val ageMs = ((nowElapsedNanos - location.elapsedRealtimeNanos) / 1_000_000L)
            .coerceAtLeast(0L)
        if (ageMs > MAX_TRUSTED_FIX_AGE_MS) {
            DengageLogger.debug("ContainmentReconciler -> skipped: fix is ${ageMs / 1000}s old")
            return emptyList()
        }

        // Staleness penalty. A known speed only ever widens the margin (a moving device covers more
        // ground than the walking assumption); it never shrinks it below the assumed floor, because
        // the speed at fix time does not promise the device stayed that slow afterwards.
        val speedMps = if (location.hasSpeed() && location.speed > 0f) {
            maxOf(location.speed.toDouble(), ASSUMED_SPEED_MPS)
        } else {
            ASSUMED_SPEED_MPS
        }
        val margin = location.accuracy.toDouble() + (ageMs / 1000.0) * speedMps
        val pending = mutableListOf<Pair<Fence, GeofenceEventType>>()

        for (fence in fenceRepository.loadAll()) {
            if (fence.geofenceId <= 0) continue

            val distance = distanceMeters(location, fence)
            val record = deviceStateRepository.getState(fence.geofenceId)
            val state = record?.state
            val deviceThinksInside = isInsideState(state)

            if (distance + margin <= fence.radiusM) {
                // Kesin içeride (en kötü uzak nokta bile radius'ta) ama tablo bilmiyor → geç/eksik enter.
                if (!deviceThinksInside) {
                    pending += fence to GeofenceEventType.ENTER
                } else if (state == FenceState.INSIDE) {
                    // Dwell repair (doc 23 İş 2): OS loiter timer'ı kaybolsa da (re-register /
                    // GMS restart / Doze) persist edilen enteredAt üzerinden dwell tamamlanır.
                    // Yalnızca "kesin içeride" dalında — margin/staleness disiplini enter/exit ile
                    // aynı. INSIDE şartı DWELL_PENDING'i (bu ziyarette zaten atıldı) dışlar;
                    // TriggerHandler dedup'ı da aynı kuralı uygular.
                    val dwellMinutes = fence.campaigns
                        .filter { it.triggerType == GeofenceTriggerType.DWELL }
                        .mapNotNull { it.dwellMinutes }
                        .maxOrNull()
                    val enteredAt = record.enteredAt
                    if (dwellMinutes != null && dwellMinutes > 0 && enteredAt != null &&
                        nowMillis - enteredAt >= dwellMinutes * 60_000L
                    ) {
                        pending += fence to GeofenceEventType.DWELL
                    }
                }
            } else if (deviceThinksInside && distance - margin > fence.radiusM * EXIT_HYSTERESIS_FACTOR) {
                // Kesin dışarıda (en kötü yakın nokta bile histerezis sınırının ötesinde) → kaçan exit.
                pending += fence to GeofenceEventType.EXIT
            }
            // Aradaki belirsiz band → dokunma, OS'a bırak.
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

        /**
         * Hard cap on fix age. Past this the staleness penalty would swamp any realistic fence
         * radius anyway, so we skip outright instead of pretending to compute something.
         */
        private const val MAX_TRUSTED_FIX_AGE_MS = 15L * 60L * 1000L

        /**
         * Distance-per-second charged against an aging fix when the real speed is unknown
         * (~walking pace). Combined with the cap this tops out at ~1.3km of extra uncertainty.
         */
        private const val ASSUMED_SPEED_MPS = 1.5
    }
}
