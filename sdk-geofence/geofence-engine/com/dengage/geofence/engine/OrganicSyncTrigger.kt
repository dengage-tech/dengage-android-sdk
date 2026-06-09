package com.dengage.geofence.engine

import com.dengage.sdk.util.DengageLogger

/**
 * Organik sync mekanizması (K6, contract §5). Push delivery + app lifecycle + movement event'lerinden
 * ETag bazlı sync check tetikler. Faz 1'de disiplinli garanti silent push yerine bu çoklu kanal kullanılır.
 *
 * `dgs_sync_check` payload field'ı opsiyonel hint'tir: hint olmasa da default sync yapılır;
 * field yalnız sunucunun yüksek frekanslı push'larda opt-out edebilmesi içindir.
 */
class OrganicSyncTrigger {

    enum class Reason { PUSH_DELIVERED, APP_FOREGROUND, MOVEMENT, BOOT, MANUAL }

    /**
     * Push payload'ına göre sync yapılmalı mı? `dgs_sync_check` açıkça "false" değilse true döner.
     */
    fun shouldSyncForPush(data: Map<String, String>?): Boolean {
        val hint = data?.get(SYNC_HINT_KEY)?.lowercase()
        val shouldSync = hint != "false"
        DengageLogger.debug("OrganicSyncTrigger -> push delivered, dgs_sync_check=$hint, sync=$shouldSync")
        return shouldSync
    }

    companion object {
        const val SYNC_HINT_KEY = "dgs_sync_check"
    }
}
