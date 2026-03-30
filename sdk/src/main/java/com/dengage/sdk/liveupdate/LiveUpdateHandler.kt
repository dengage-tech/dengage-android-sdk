package com.dengage.sdk.liveupdate

import android.content.Context

/**
 * Handler interface for live update events.
 *
 * Register a handler for each activity type via [DengageLiveUpdateManager.register].
 * The handler receives the parsed [LiveUpdatePayload] and is responsible for
 * building and posting (or cancelling) the notification.
 *
 * ```kotlin
 * class MyDeliveryHandler : LiveUpdateHandler {
 *     override fun onUpdate(context: Context, payload: LiveUpdatePayload) {
 *         when (payload.event) {
 *             LiveUpdateEvent.START, LiveUpdateEvent.UPDATE -> showNotification(context, payload)
 *             LiveUpdateEvent.END -> cancelNotification(context, payload.activityId)
 *         }
 *     }
 * }
 *
 * DengageLiveUpdateManager.register("delivery", MyDeliveryHandler())
 * ```
 */
public interface LiveUpdateHandler {
    /**
     * Called when a live update event is received for the registered activity type.
     *
     * @param context Application context
     * @param payload Parsed live update payload including event, activityId, and content_state
     */
    fun onUpdate(context: Context, payload: LiveUpdatePayload)
}
