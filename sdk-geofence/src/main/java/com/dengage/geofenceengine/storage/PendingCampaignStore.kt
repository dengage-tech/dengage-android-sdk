package com.dengage.geofenceengine.storage

import android.content.Context
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventType

/**
 * State-only tüketilmiş ama kampanyası atılmamış geçişlerin "kampanya borcu" kaydı.
 *
 * `fireCampaigns=false` bir reconcile (sync-wake / cross-fence tamir) bir geçişi state'e işlerse,
 * aynı geçişin sonraki gerçek OS callback'i dedup'a takılır ve kampanya kalıcı kaybolurdu.
 * Bu store, borcu persist eder; ilk `fireCampaigns=true` tekrar (gerçek OS callback'i veya
 * movement reeval) state'e dokunmadan borcu öder.
 *
 * Process ölümüne dayanması gerekir (broadcast receiver process'leri kısa ömürlü) → SharedPreferences.
 */
internal class PendingCampaignStore(context: Context) {

    private val prefs by lazy {
        context.applicationContext.getSharedPreferences(PREFS_FILE, Context.MODE_PRIVATE)
    }

    private fun key(geofenceId: Int, eventType: GeofenceEventType): String =
        "$geofenceId:${eventType.wireValue}"

    /** Geçiş state-only tüketildi; kampanya borcu tespit anıyla birlikte yazılır. */
    fun mark(geofenceId: Int, eventType: GeofenceEventType, detectedAt: Long) {
        prefs.edit().putLong(key(geofenceId, eventType), detectedAt).apply()
    }

    /**
     * Borç varsa tespit anını döner ve siler; yoksa null. TTL aşımı da null — bayat borç ödenmez
     * (occurredAt olarak sunucuya gidecek zaman makul kalmalı).
     */
    fun consume(
        geofenceId: Int,
        eventType: GeofenceEventType,
        now: Long = System.currentTimeMillis()
    ): Long? {
        val k = key(geofenceId, eventType)
        val at = prefs.getLong(k, -1L)
        if (at <= 0L) return null
        prefs.edit().remove(k).apply()
        return if (now - at <= TTL_MS) at else null
    }

    /** Fence için tüm borçları temizle — yeni gerçek geçiş eski ziyaretin borçlarını geçersiz kılar. */
    fun clearFence(geofenceId: Int) {
        val editor = prefs.edit()
        prefs.all.keys
            .filter { it.startsWith("$geofenceId:") }
            .forEach { editor.remove(it) }
        editor.apply()
    }

    companion object {
        private const val PREFS_FILE = "dengage_geofence_pending_campaigns"

        /** Borcun ödenebilir kalacağı azami süre. */
        private const val TTL_MS = 6L * 60L * 60L * 1000L
    }
}
