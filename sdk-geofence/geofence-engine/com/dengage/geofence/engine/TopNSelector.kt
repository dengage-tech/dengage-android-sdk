package com.dengage.geofence.engine

import com.dengage.geofence.engine.storage.model.Fence
import kotlin.math.asin
import kotlin.math.cos
import kotlin.math.pow
import kotlin.math.sin
import kotlin.math.sqrt

/**
 * Cihaz konumuna göre OS'a register edilecek top-N fence seçimi (doc 21 §6.5).
 * `active_now = true` olanlar + 1 saat içinde aktif olacaklar (pre-warm) dahil edilir;
 * haversine'e göre en yakın N tanesi seçilir.
 */
class TopNSelector {

    fun select(
        fences: List<Fence>,
        lat: Double,
        lon: Double,
        topN: Int,
        now: Long = System.currentTimeMillis()
    ): List<Fence> {
        return fences
            .filter { isActiveOrPreWarm(it, now) }
            .sortedBy { haversineKm(lat, lon, it.latitude, it.longitude) }
            .take(topN)
    }

    private fun isActiveOrPreWarm(fence: Fence, now: Long): Boolean {
        if (fence.activeNow) return true
        // Pre-warm: 1 saat içinde "active" olacaksa register et (K9 initialTrigger ile anında yakalanır)
        val next = fence.nextStateChangeAtMillis ?: return false
        val becomesActive = fence.nextStateChangeTo == null || fence.nextStateChangeTo == "active"
        return becomesActive && next in now..(now + PRE_WARM_WINDOW_MS)
    }

    private fun haversineKm(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double {
        val r = 6372.8
        val dLat = Math.toRadians(lat2 - lat1)
        val dLon = Math.toRadians(lon2 - lon1)
        val a = sin(dLat / 2).pow(2.0) +
            sin(dLon / 2).pow(2.0) * cos(Math.toRadians(lat1)) * cos(Math.toRadians(lat2))
        return r * 2 * asin(sqrt(a))
    }

    companion object {
        private const val PRE_WARM_WINDOW_MS = 60 * 60 * 1000L
    }
}
