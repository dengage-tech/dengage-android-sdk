package com.dengage.geofence.engine.storage

/** Son ETag / sync zamanı metadata'sı (doc 21 §6.2). Default impl SharedPreferences. */
interface SyncMetadataRepository {
    var lastETag: String?
    var lastSyncedAt: Long?
    var lastHeartbeatAt: Long?
}
