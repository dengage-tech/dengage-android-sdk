package com.dengage.sdk.liveupdate

import android.app.Notification
import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.graphics.Color
import android.graphics.drawable.Icon
import android.os.Build
import android.os.Bundle
import android.view.View
import android.widget.RemoteViews
import androidx.annotation.RequiresApi
import androidx.core.app.NotificationCompat
import com.dengage.sdk.R

/**
 * SDK-level manager for Android Live Update notifications.
 *
 * - **API < 36**: RemoteViews tabanlı özel layout (delivery + sports şablonları)
 * - **API 36+**: `Notification.ProgressStyle` + promoted ongoing
 *   (kilit ekranı, status bar chip, her zaman açık ekran)
 *
 * ### Kullanım
 * ```kotlin
 * // Yerleşik delivery şablonu
 * Dengage.liveUpdateManager.showDeliveryUpdate(context, DeliveryUpdate(...))
 *
 * // Yerleşik sports şablonu
 * Dengage.liveUpdateManager.showSportsUpdate(context, SportsUpdate(...))
 *
 * // Tamamen özel işlem için callback
 * Dengage.liveUpdateManager.setCallback(object : LiveUpdateCallback {
 *     override fun onLiveUpdate(context: Context, type: String, data: Map<String, String>) { ... }
 * })
 * ```
 */
object DengageLiveUpdateManager {

    private const val CHANNEL_ID = "den_live_update_channel"
    private const val CHANNEL_NAME = "Live Updates"

    const val NOTIFICATION_ID_DELIVERY = 2001
    const val NOTIFICATION_ID_SPORTS = 2002

    /** Ayarlandığında yerleşik şablonlar atlanır, her mesaj bu callback'e iletilir. */
    @Volatile
    var callback: LiveUpdateCallback? = null
        private set

    // -------------------------------------------------------------------------
    // Veri modelleri
    // -------------------------------------------------------------------------

    enum class DeliveryStatus(val label: String, val step: Int) {
        ORDER_RECEIVED("Sipariş Alındı", 1),
        PREPARING("Hazırlanıyor", 2),
        ON_THE_WAY("Yolda", 3),
        DELIVERED("Teslim Edildi", 4)
    }

    data class DeliveryUpdate(
        val orderId: String,
        val status: DeliveryStatus,
        val estimatedTime: String
    )

    data class SportsUpdate(
        val team1: String,
        val team2: String,
        val score1: Int,
        val score2: Int,
        val matchTime: String,
        val period: String
    )

    // -------------------------------------------------------------------------
    // Genel API
    // -------------------------------------------------------------------------

    fun setCallback(callback: LiveUpdateCallback?) {
        this.callback = callback
    }

    fun showDeliveryUpdate(context: Context, update: DeliveryUpdate) {
        ensureChannel(context)
        val notification = if (Build.VERSION.SDK_INT >= 36) {
            buildDeliveryApi36(context, update)
        } else {
            buildDeliveryLegacy(context, update)
        }
        notify(context, NOTIFICATION_ID_DELIVERY, notification)
    }

    fun showSportsUpdate(context: Context, update: SportsUpdate) {
        ensureChannel(context)
        val notification = if (Build.VERSION.SDK_INT >= 36) {
            buildSportsApi36(context, update)
        } else {
            buildSportsLegacy(context, update)
        }
        notify(context, NOTIFICATION_ID_SPORTS, notification)
    }

    fun dismissDeliveryUpdate(context: Context) = cancel(context, NOTIFICATION_ID_DELIVERY)

    fun dismissSportsUpdate(context: Context) = cancel(context, NOTIFICATION_ID_SPORTS)

    // -------------------------------------------------------------------------
    // API 36+ — Notification.ProgressStyle + Promoted Ongoing
    // -------------------------------------------------------------------------

    @RequiresApi(36)
    private fun buildDeliveryApi36(context: Context, update: DeliveryUpdate): Notification {
        val step = update.status.step
        val isDelivered = update.status == DeliveryStatus.DELIVERED

        // 4 segment — tamamlananlar mor, bekleyenler gri
        val segments = (1..4).map { i ->
            Notification.ProgressStyle.Segment(1).apply {
                setColor(if (i <= step) 0xFF6200EE.toInt() else 0xFFDDDDDD.toInt())
            }
        }

        // Segment sınırlarındaki nokta işaretleri (pozisyon 1, 2, 3)
        val points = (1..3).map { i ->
            Notification.ProgressStyle.Point(i).apply {
                setColor(if (i < step) 0xFF6200EE.toInt() else 0xFFDDDDDD.toInt())
            }
        }

        val progressStyle = Notification.ProgressStyle()
            .setProgress(step)
            .setProgressSegments(segments)
            .setProgressPoints(points)

        val subtitleText = if (isDelivered) {
            "Sipariş teslim edildi"
        } else {
            "${update.status.label} · Tahmini varış: ${update.estimatedTime}"
        }

        val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        val canPromote = !isDelivered && nm.canPostPromotedNotifications()

        return Notification.Builder(context, CHANNEL_ID)
            .setSmallIcon(Icon.createWithResource(context, context.applicationInfo.icon))
            .setContentTitle("Sipariş #${update.orderId}")
            .setContentText(subtitleText)
            .setStyle(progressStyle)
            .setOngoing(!isDelivered)
            .setOnlyAlertOnce(true)
            .setContentIntent(buildMainIntent(context, NOTIFICATION_ID_DELIVERY))
            .apply {
                if (canPromote) {
                    addExtras(Bundle().apply {
                        putBoolean("android.requestPromotedOngoing", true)
                    })
                }
            }
            .build()
    }

    @RequiresApi(36)
    private fun buildSportsApi36(context: Context, update: SportsUpdate): Notification {
        val isFinished = update.period.contains("Bitti", ignoreCase = true)
        val scoreText = "${update.team1}  ${update.score1} - ${update.score2}  ${update.team2}"
        val statusText = "${update.matchTime} · ${update.period}"

        val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        val canPromote = !isFinished && nm.canPostPromotedNotifications()

        return Notification.Builder(context, CHANNEL_ID)
            .setSmallIcon(Icon.createWithResource(context, context.applicationInfo.icon))
            .setContentTitle(scoreText)
            .setContentText(statusText)
            .setStyle(Notification.BigTextStyle().bigText("$scoreText\n$statusText"))
            .setOngoing(!isFinished)
            .setOnlyAlertOnce(true)
            .setContentIntent(buildMainIntent(context, NOTIFICATION_ID_SPORTS))
            .apply {
                if (canPromote) {
                    addExtras(Bundle().apply {
                        putBoolean("android.requestPromotedOngoing", true)
                    })
                }
            }
            .build()
    }

    // -------------------------------------------------------------------------
    // API < 36 — RemoteViews tabanlı özel layout
    // -------------------------------------------------------------------------

    private fun buildDeliveryLegacy(context: Context, update: DeliveryUpdate): Notification {
        val isDelivered = update.status == DeliveryStatus.DELIVERED

        val expandedView = RemoteViews(context.packageName, R.layout.den_notification_live_delivery).apply {
            setTextViewText(R.id.den_tvLiveDeliveryOrderId, "#${update.orderId}")
            setTextViewText(R.id.den_tvLiveDeliveryStatus, update.status.label)
            setTextViewText(R.id.den_tvLiveDeliveryEta, update.estimatedTime)
            setProgressBar(R.id.den_progressLiveDelivery, 4, update.status.step, false)

            val dotIds = listOf(R.id.den_vStep1, R.id.den_vStep2, R.id.den_vStep3, R.id.den_vStep4)
            val labelIds = listOf(R.id.den_tvStep1, R.id.den_tvStep2, R.id.den_tvStep3, R.id.den_tvStep4)

            dotIds.forEachIndexed { index, dotId ->
                val s = index + 1
                val drawable = when {
                    s < update.status.step  -> R.drawable.den_live_dot_done
                    s == update.status.step -> R.drawable.den_live_dot_active
                    else                    -> R.drawable.den_live_dot_inactive
                }
                setInt(dotId, "setBackgroundResource", drawable)
            }
            labelIds.forEachIndexed { index, labelId ->
                val color = if (index + 1 <= update.status.step)
                    Color.parseColor("#FF6200EE") else Color.parseColor("#FFAAAAAA")
                setTextColor(labelId, color)
            }

            setViewVisibility(R.id.den_pbLiveDeliverySpinner, if (isDelivered) View.GONE else View.VISIBLE)
            setViewVisibility(R.id.den_tvLiveDeliveryEtaLabel, if (isDelivered) View.GONE else View.VISIBLE)
            setViewVisibility(R.id.den_tvLiveDeliveryEta, if (isDelivered) View.GONE else View.VISIBLE)
        }

        return NotificationCompat.Builder(context, CHANNEL_ID)
            .setSmallIcon(context.applicationInfo.icon)
            .setContentTitle("Sipariş Takibi")
            .setContentText(update.status.label)
            .setCustomBigContentView(expandedView)
            .setStyle(NotificationCompat.DecoratedCustomViewStyle())
            .setOngoing(!isDelivered)
            .setOnlyAlertOnce(true)
            .setContentIntent(buildMainIntent(context, NOTIFICATION_ID_DELIVERY))
            .build()
    }

    private fun buildSportsLegacy(context: Context, update: SportsUpdate): Notification {
        val isFinished = update.period.contains("Bitti", ignoreCase = true)

        val expandedView = RemoteViews(context.packageName, R.layout.den_notification_live_sports).apply {
            setTextViewText(R.id.den_tvLiveSportsTeam1, update.team1)
            setTextViewText(R.id.den_tvLiveSportsTeam2, update.team2)
            setTextViewText(R.id.den_tvLiveSportsScore1, update.score1.toString())
            setTextViewText(R.id.den_tvLiveSportsScore2, update.score2.toString())
            setTextViewText(R.id.den_tvLiveSportsTime, update.matchTime)
            setTextViewText(R.id.den_tvLiveSportsPeriod, update.period)

            setInt(R.id.den_tvLiveSportsTime, "setBackgroundColor",
                if (isFinished) Color.parseColor("#FF888888") else Color.parseColor("#FFCC0000"))
            setViewVisibility(R.id.den_pbLiveSportsIndicator, if (isFinished) View.GONE else View.VISIBLE)
        }

        val scoreText = "${update.team1} ${update.score1} - ${update.score2} ${update.team2}"
        return NotificationCompat.Builder(context, CHANNEL_ID)
            .setSmallIcon(context.applicationInfo.icon)
            .setContentTitle("Canlı Skor")
            .setContentText(scoreText)
            .setCustomBigContentView(expandedView)
            .setStyle(NotificationCompat.DecoratedCustomViewStyle())
            .setOngoing(!isFinished)
            .setOnlyAlertOnce(true)
            .setContentIntent(buildMainIntent(context, NOTIFICATION_ID_SPORTS))
            .build()
    }

    // -------------------------------------------------------------------------
    // FCM routing
    // -------------------------------------------------------------------------

    internal fun handleFromFcmData(context: Context, data: Map<String, String>) {
        val type = data["live_update_type"] ?: return

        val cb = callback
        if (cb != null) {
            cb.onLiveUpdate(context, type, data)
            return
        }

        when (type) {
            "delivery" -> {
                val statusName = data["delivery_status"] ?: return
                val status = runCatching { DeliveryStatus.valueOf(statusName) }.getOrNull() ?: return
                showDeliveryUpdate(
                    context,
                    DeliveryUpdate(
                        orderId = data["order_id"] ?: "",
                        status = status,
                        estimatedTime = data["estimated_time"] ?: ""
                    )
                )
            }
            "sports" -> {
                showSportsUpdate(
                    context,
                    SportsUpdate(
                        team1 = data["team1"] ?: "",
                        team2 = data["team2"] ?: "",
                        score1 = data["score1"]?.toIntOrNull() ?: 0,
                        score2 = data["score2"]?.toIntOrNull() ?: 0,
                        matchTime = data["match_time"] ?: "",
                        period = data["period"] ?: ""
                    )
                )
            }
            "dismiss" -> {
                when (data["dismiss_target"]) {
                    "delivery" -> dismissDeliveryUpdate(context)
                    "sports"   -> dismissSportsUpdate(context)
                }
            }
        }
    }

    // -------------------------------------------------------------------------
    // Yardımcılar
    // -------------------------------------------------------------------------

    private fun ensureChannel(context: Context) {
        val manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        if (manager.getNotificationChannel(CHANNEL_ID) != null) return
        val channel = NotificationChannel(CHANNEL_ID, CHANNEL_NAME, NotificationManager.IMPORTANCE_DEFAULT).apply {
            description = "Dengage Live Update Notifications"
            setSound(null, null)
        }
        manager.createNotificationChannel(channel)
    }

    private fun buildMainIntent(context: Context, requestCode: Int): PendingIntent {
        val launchIntent = context.packageManager.getLaunchIntentForPackage(context.packageName) ?: Intent()
        return PendingIntent.getActivity(context, requestCode, launchIntent, PendingIntent.FLAG_IMMUTABLE)
    }

    private fun notify(context: Context, id: Int, notification: Notification) {
        val manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        manager.notify(id, notification)
    }

    private fun cancel(context: Context, id: Int) {
        val manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        manager.cancel(id)
    }
}
