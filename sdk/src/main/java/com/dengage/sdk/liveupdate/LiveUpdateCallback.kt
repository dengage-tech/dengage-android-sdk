package com.dengage.sdk.liveupdate

import android.content.Context

/**
 * Optional callback for fully custom live update handling.
 *
 * Register via [DengageLiveUpdateManager.setCallback].
 * When set, the default delivery/sports templates are bypassed entirely
 * and this callback receives the raw FCM data map for every live update message.
 */
interface LiveUpdateCallback {
    /**
     * Called when a push message with `live_update_type` is received.
     *
     * @param context Application context
     * @param type    The value of `live_update_type` (e.g. "delivery", "sports", "dismiss")
     * @param data    Full FCM data payload as a key-value map
     */
    fun onLiveUpdate(context: Context, type: String, data: Map<String, String>)
}
