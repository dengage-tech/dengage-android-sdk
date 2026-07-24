package com.dengage.geofenceengine.storage

/** Son ETag / sync zamanı metadata'sı (doc 21 §6.2). Default impl SharedPreferences. */
interface SyncMetadataRepository {
    var lastETag: String?
    var lastSyncedAt: Long?
    var lastHeartbeatAt: Long?

    /** Silent push (sourceType=geofence) ile yapılan son resync zamanı. */
    var lastSilentPushAt: Long?

    /**
     * Wake-up cap pause başlangıcı (epoch millis); null = pause yok.
     * Persist edilir ki process ölüp yeniden doğduğunda pause penceresi doğru değerlendirilebilsin
     * (aksi halde `attemptResume()` taze bellekte `paused=false` görüp erken döner).
     */
    var wakeupPausedAt: Long?
}
