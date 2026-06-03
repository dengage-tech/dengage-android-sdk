package com.dengage.sdk.liveupdate

import android.app.Notification
import android.app.NotificationChannel
import android.app.NotificationManager
import android.content.Context
import android.os.Build
import android.os.Handler
import android.os.Looper

/**
 * Handler for live update notifications of a single activity type.
 *
 * Implementations only need to provide the notification's **visual content** via
 * [buildNotification] and declare the [channelId] / [channelName] to post on.
 * The SDK's default [onUpdate] implementation takes care of the shared plumbing:
 * lifecycle handling, notification channel creation, posting, cancellation, and
 * auto-dismissal (including [LiveUpdatePayload.dismissalDate] scheduling).
 *
 * Register a handler for each activity type via [DengageLiveUpdateManager.register].
 *
 * ```kotlin
 * class MyDeliveryHandler : LiveUpdateHandler {
 *     override val channelId = "my_delivery_channel"
 *     override val channelName = "Delivery"
 *
 *     override fun buildNotification(context: Context, payload: LiveUpdatePayload): Notification? {
 *         val cs = payload.contentState
 *         return Notification.Builder(context, channelId)
 *             .setContentTitle("Order #${cs["order_id"]}")
 *             // ... visual content only
 *             .build()
 *     }
 * }
 *
 * DengageLiveUpdateManager.register("delivery", MyDeliveryHandler())
 * ```
 *
 * Advanced hosts that need full control over the notification lifecycle may
 * override [onUpdate] directly instead of implementing [buildNotification].
 */
public interface LiveUpdateHandler {

    /** Notification channel id used when posting this handler's notifications. */
    val channelId: String

    /** User-visible notification channel name (used when the channel is created). */
    val channelName: String

    /** Optional channel description shown in the system notification settings. */
    val channelDescription: String? get() = null

    /**
     * Build the notification to display for the given [payload].
     *
     * Implementations are only responsible for the **visual content** (title, text,
     * style, etc.). Return `null` if there is nothing to display for this payload.
     *
     * Channel creation, posting, cancellation and dismissal are handled by [onUpdate].
     *
     * @param context Application context
     * @param payload Parsed live update payload including event, activityId, and content_state
     */
    fun buildNotification(context: Context, payload: LiveUpdatePayload): Notification?

    /**
     * Called by [DengageLiveUpdateManager] when a live update event is received for
     * the registered activity type.
     *
     * The default implementation handles the full notification lifecycle and should
     * rarely be overridden — implement [buildNotification] instead. Override only if
     * you need complete control over how notifications are posted and cancelled.
     *
     * @param context Application context
     * @param payload Parsed live update payload including event, activityId, and content_state
     */
    fun onUpdate(context: Context, payload: LiveUpdatePayload) {
        val notificationId = payload.activityId.hashCode()
        val dismissal = payload.dismissalDate

        // END with no content → only dismiss the existing notification.
        if (payload.event == LiveUpdateEvent.END && payload.contentState.isEmpty()) {
            dismissNotification(context, notificationId, dismissal)
            return
        }

        val notification = buildNotification(context, payload)
        if (notification == null) {
            // Nothing to display; still honour dismissal on END.
            if (payload.event == LiveUpdateEvent.END) {
                dismissNotification(context, notificationId, dismissal)
            }
            return
        }

        ensureChannel(context, channelId, channelName, channelDescription)

        val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        nm.notify(notificationId, notification)

        when {
            // END with content → show the final state, then dismiss.
            payload.event == LiveUpdateEvent.END ->
                dismissNotification(context, notificationId, dismissal)
            // START/UPDATE with a dismissal date → auto-dismiss at that time.
            dismissal != null ->
                scheduleCancel(context, notificationId, dismissal)
        }
    }
}

// -----------------------------------------------------------------------------
// Shared lifecycle plumbing — file-private, not part of the public interface API.
// -----------------------------------------------------------------------------

/** Cancel now, or schedule cancellation at [dismissalDateSec] (epoch seconds) when provided. */
private fun dismissNotification(context: Context, notificationId: Int, dismissalDateSec: Long?) {
    if (dismissalDateSec != null) {
        scheduleCancel(context, notificationId, dismissalDateSec)
    } else {
        cancelNotification(context, notificationId)
    }
}

private fun cancelNotification(context: Context, notificationId: Int) {
    val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
    nm.cancel(notificationId)
}

private fun scheduleCancel(context: Context, notificationId: Int, dismissalDateSec: Long) {
    val delayMs = (dismissalDateSec * 1000L) - System.currentTimeMillis()
    if (delayMs <= 0) {
        cancelNotification(context, notificationId)
    } else {
        Handler(Looper.getMainLooper()).postDelayed({ cancelNotification(context, notificationId) }, delayMs)
    }
}

private fun ensureChannel(
    context: Context,
    channelId: String,
    channelName: String,
    channelDescription: String?
) {
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) return
    val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
    if (nm.getNotificationChannel(channelId) != null) return
    val channel = NotificationChannel(channelId, channelName, NotificationManager.IMPORTANCE_DEFAULT).apply {
        if (channelDescription != null) description = channelDescription
        setSound(null, null)
        lockscreenVisibility = Notification.VISIBILITY_PUBLIC
    }
    nm.createNotificationChannel(channel)
}
