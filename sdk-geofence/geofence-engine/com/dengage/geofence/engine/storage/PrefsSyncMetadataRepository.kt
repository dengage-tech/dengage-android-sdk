package com.dengage.geofence.engine.storage

import android.content.Context
import com.dengage.sdk.util.ContextHolder

/**
 * Default [SyncMetadataRepository] impl — kendi SharedPreferences dosyasında.
 * (sdk modülünün `Prefs.preferences` üyesi `internal` olduğu için cross-module erişilemez;
 * engine kendi store'unu kullanır.)
 */
internal class PrefsSyncMetadataRepository : SyncMetadataRepository {

    private val prefs by lazy {
        ContextHolder.context.getSharedPreferences(PREFS_FILE, Context.MODE_PRIVATE)
    }

    override var lastETag: String?
        get() = prefs.getString(KEY_ETAG, null)
        set(value) = prefs.edit().putString(KEY_ETAG, value).apply()

    override var lastSyncedAt: Long?
        get() = prefs.getLong(KEY_SYNCED_AT, 0L).takeIf { it > 0 }
        set(value) {
            prefs.edit().putLong(KEY_SYNCED_AT, value ?: 0L).apply()
        }

    override var lastHeartbeatAt: Long?
        get() = prefs.getLong(KEY_HEARTBEAT_AT, 0L).takeIf { it > 0 }
        set(value) {
            prefs.edit().putLong(KEY_HEARTBEAT_AT, value ?: 0L).apply()
        }

    companion object {
        private const val PREFS_FILE = "dengage_geofence_engine_prefs"
        private const val KEY_ETAG = "etag"
        private const val KEY_SYNCED_AT = "synced_at"
        private const val KEY_HEARTBEAT_AT = "heartbeat_at"
    }
}
