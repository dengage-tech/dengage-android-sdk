package com.dengage.geofenceengine

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
import com.dengage.sdk.push.getSmallIconId
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger

/**
 * Offline trigger anında cache'lenen [OfflinePushContent] ile local notification gösterir (K10).
 * Network gerekmez — title/body/deeplink fence ile birlikte cache'lenmiştir.
 */
class LocalNotificationFirer(private val context: Context) {

    fun fire(content: OfflinePushContent, geofenceId: Int, campaignId: Int?) {
        val title = content.title
        val body = content.body
        if (title.isNullOrBlank() && body.isNullOrBlank()) {
            DengageLogger.debug("LocalNotificationFirer -> empty content, skipping")
            return
        }

        ensureChannel()

        val builder = NotificationCompat.Builder(context, Constants.NOTIFICATION_CHANNEL_ID)
            .setSmallIcon(smallIconId())
            .setContentTitle(title ?: "")
            .setContentText(body ?: "")
            .setStyle(NotificationCompat.BigTextStyle().bigText(body ?: ""))
            .setPriority(NotificationCompat.PRIORITY_HIGH)
            .setAutoCancel(true)

        contentIntent(content.deepLink, geofenceId)?.let { builder.setContentIntent(it) }

        val notificationId = campaignId ?: geofenceId
        try {
            NotificationManagerCompat.from(context).notify(notificationId, builder.build())
            DengageLogger.debug("LocalNotificationFirer -> fired notification id=$notificationId (fence=$geofenceId)")
        } catch (e: SecurityException) {
            // POST_NOTIFICATIONS izni yoksa (Android 13+)
            DengageLogger.error("LocalNotificationFirer -> notify failed: ${e.message}")
            GeofenceDebugLogger.error(
                "Geofence local notification failed",
                mapOf("error" to (e.message ?: "unknown"), "geofenceId" to geofenceId.toString())
            )
        }
    }

    /**
     * Bildirime tıklanınca açılacak intent. Deeplink varsa ACTION_VIEW ile ona,
     * yoksa uygulamanın launcher activity'sine yönlendirir (deeplink boşken de uygulama açılsın).
     */
    private fun contentIntent(deepLink: String?, geofenceId: Int): PendingIntent? {
        val intent = if (!deepLink.isNullOrBlank()) {
            Intent(Intent.ACTION_VIEW, Uri.parse(deepLink)).apply {
                addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
            }
        } else {
            // Deeplink yok → uygulamayı aç.
            context.packageManager.getLaunchIntentForPackage(context.packageName)?.apply {
                addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
            }
        } ?: return null

        var flags = PendingIntent.FLAG_UPDATE_CURRENT
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            flags = flags or PendingIntent.FLAG_IMMUTABLE
        }
        return try {
            PendingIntent.getActivity(context, geofenceId, intent, flags)
        } catch (e: Exception) {
            null
        }
    }

    /**
     * Normal push ile aynı small icon'u kullan (`den_push_small_icon` meta-data'sı, yoksa uygulama
     * ikonu). Böylece offline geofence bildirimi jenerik sistem ikonu yerine uygulama markasıyla çıkar.
     * `getSmallIconId` geçersiz (-1) dönerse sistem harita ikonuna düş — bildirim ikonsuz kalmasın.
     */
    private fun smallIconId(): Int {
        val id = context.getSmallIconId()
        return if (id > 0) id else android.R.drawable.ic_dialog_map
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
