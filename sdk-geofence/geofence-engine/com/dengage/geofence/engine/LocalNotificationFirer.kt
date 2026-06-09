package com.dengage.geofence.engine

import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Build
import androidx.core.app.NotificationCompat
import androidx.core.app.NotificationManagerCompat
import com.dengage.sdk.domain.geofence.model.sync.OfflinePushContent
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger

/**
 * Offline trigger anında cache'lenen [OfflinePushContent] ile local notification gösterir (K10).
 * Network gerekmez — title/body/deeplink fence ile birlikte cache'lenmiştir.
 */
class LocalNotificationFirer(private val context: Context) {

    fun fire(content: OfflinePushContent, fenceId: Int, campaignId: Int?) {
        val title = content.title
        val body = content.body
        if (title.isNullOrBlank() && body.isNullOrBlank()) {
            DengageLogger.debug("LocalNotificationFirer -> empty content, skipping")
            return
        }

        ensureChannel()

        val builder = NotificationCompat.Builder(context, Constants.NOTIFICATION_CHANNEL_ID)
            .setSmallIcon(android.R.drawable.ic_dialog_map)
            .setContentTitle(title ?: "")
            .setContentText(body ?: "")
            .setStyle(NotificationCompat.BigTextStyle().bigText(body ?: ""))
            .setPriority(NotificationCompat.PRIORITY_HIGH)
            .setAutoCancel(true)

        deepLinkIntent(content.deepLink, fenceId)?.let { builder.setContentIntent(it) }

        val notificationId = campaignId ?: fenceId
        try {
            NotificationManagerCompat.from(context).notify(notificationId, builder.build())
            DengageLogger.debug("LocalNotificationFirer -> fired notification id=$notificationId (fence=$fenceId)")
        } catch (e: SecurityException) {
            // POST_NOTIFICATIONS izni yoksa (Android 13+)
            DengageLogger.error("LocalNotificationFirer -> notify failed: ${e.message}")
        }
    }

    private fun deepLinkIntent(deepLink: String?, fenceId: Int): PendingIntent? {
        if (deepLink.isNullOrBlank()) return null
        val intent = Intent(Intent.ACTION_VIEW, Uri.parse(deepLink)).apply {
            addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        }
        var flags = PendingIntent.FLAG_UPDATE_CURRENT
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            flags = flags or PendingIntent.FLAG_IMMUTABLE
        }
        return try {
            PendingIntent.getActivity(context, fenceId, intent, flags)
        } catch (e: Exception) {
            null
        }
    }

    private fun ensureChannel() {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) return
        val manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        if (manager.getNotificationChannel(Constants.NOTIFICATION_CHANNEL_ID) == null) {
            val channel = NotificationChannel(
                Constants.NOTIFICATION_CHANNEL_ID,
                Constants.NOTIFICATION_CHANNEL_NAME,
                NotificationManager.IMPORTANCE_HIGH
            )
            manager.createNotificationChannel(channel)
        }
    }
}
