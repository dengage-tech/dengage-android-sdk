package com.dengage.sdk.push

import android.app.Notification
import android.app.NotificationManager
import android.content.Context
import android.os.Build
import androidx.core.app.NotificationCompat
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.DengageLogger
import kotlin.math.max

/**
 * Makes the launcher badge behave like the iOS SDK.
 *
 * Android has no app-wide badge value: launchers that show a count add up `number` of the app's
 * active notifications (a notification without one counts as 1). The number given to a new
 * notification is chosen so that this sum becomes:
 * - `isIncrementalBadge`: the current badge + 1
 * - `badgeCount` N > 0: N; when the other notifications already add up to N or more,
 *   the lowest possible value (current badge + 1), since visible notifications cannot be counted lower
 * - otherwise the notification counts as 1, as before.
 */
internal object NotificationBadge {

    private const val TAG = "NotificationBadge:"

    fun apply(
        context: Context,
        notificationBuilder: NotificationCompat.Builder,
        message: Message,
        notificationId: Int
    ) {
        val badgeCount = message.badgeCount ?: 0
        val number = when {
            message.isIncrementalBadge == true -> 1
            badgeCount > 0 -> max(badgeCount - otherNotificationsBadge(context, notificationId), 1)
            else -> return
        }
        DengageLogger.verbose("$TAG notification number set to $number")
        notificationBuilder.setBadgeIconType(NotificationCompat.BADGE_ICON_SMALL)
        notificationBuilder.setNumber(number)
    }

    /** The badge the app's other active notifications add up to; 0 when it cannot be read (below API 23). */
    private fun otherNotificationsBadge(context: Context, notificationId: Int): Int {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.M) return 0
        val manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as? NotificationManager
            ?: return 0
        return try {
            manager.activeNotifications
                .filterNot { it.id == notificationId && it.tag == null } // the notification being replaced
                .filter { it.notification.flags and Notification.FLAG_GROUP_SUMMARY == 0 }
                .sumOf { max(it.notification.number, 1) }
        } catch (e: Exception) {
            DengageLogger.error("$TAG active notifications could not be read: ${e.message}")
            0
        }
    }
}
