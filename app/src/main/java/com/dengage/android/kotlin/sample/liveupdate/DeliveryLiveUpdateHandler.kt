package com.dengage.android.kotlin.sample.liveupdate

import android.app.Notification
import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.os.Handler
import android.os.Looper
import android.graphics.Color
import android.graphics.drawable.Icon
import android.os.Build
import android.os.Bundle
import android.view.View
import android.widget.RemoteViews
import androidx.annotation.RequiresApi
import androidx.core.app.NotificationCompat
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.ui.activity.MainActivity
import com.dengage.sdk.liveupdate.LiveUpdateEvent
import com.dengage.sdk.liveupdate.LiveUpdateHandler
import com.dengage.sdk.liveupdate.LiveUpdatePayload

class DeliveryLiveUpdateHandler : LiveUpdateHandler {

    companion object {
        const val CHANNEL_ID = "den_live_update_delivery"
    }

    enum class DeliveryStatus(val label: String, val step: Int) {
        ORDER_RECEIVED("Sipariş Alındı", 1),
        PREPARING("Hazırlanıyor", 2),
        ON_THE_WAY("Yolda", 3),
        DELIVERED("Teslim Edildi", 4)
    }

    override fun onUpdate(context: Context, payload: LiveUpdatePayload) {
        val notificationId = payload.activityId.hashCode()
        val dismissal = payload.dismissalDate

        if (payload.event == LiveUpdateEvent.END && payload.contentState.isEmpty()) {
            // İçerik yoksa sadece kapat
            if (dismissal != null) {
                scheduleCancel(context, notificationId, dismissal)
            } else {
                cancel(context, notificationId)
            }
            return
        }

        val cs = payload.contentState
        val statusName = cs["delivery_status"] ?: return
        val status = runCatching { DeliveryStatus.valueOf(statusName) }.getOrNull() ?: return
        val orderId = cs["order_id"] ?: ""
        val eta = cs["estimated_time"] ?: ""

        ensureChannel(context)

        val notification = if (Build.VERSION.SDK_INT >= 36) {
            buildApi36(context, orderId, status, eta, notificationId, dismissal)
        } else {
            buildLegacy(context, orderId, status, eta, notificationId, dismissal)
        }
        val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        nm.notify(notificationId, notification)

        // END event + içerik varsa: bildirimi güncelle, sonra dismissalDate'te kapat
        if (payload.event == LiveUpdateEvent.END) {
            if (dismissal != null) {
                scheduleCancel(context, notificationId, dismissal)
            } else {
                cancel(context, notificationId)
            }
        }
    }

    fun cancel(context: Context, notificationId: Int) {
        val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        nm.cancel(notificationId)
    }

    // -------------------------------------------------------------------------
    // API 36+
    // -------------------------------------------------------------------------

    @RequiresApi(36)
    private fun buildApi36(
        context: Context, orderId: String, status: DeliveryStatus,
        eta: String, notificationId: Int, dismissalDate: Long?
    ): Notification {
        val step = status.step
        val isDelivered = status == DeliveryStatus.DELIVERED

        val segments = (1..4).map { i ->
            Notification.ProgressStyle.Segment(1).apply {
                setColor(if (i <= step) 0xFF6200EE.toInt() else 0xFFDDDDDD.toInt())
            }
        }
        val points = (1..3).map { i ->
            Notification.ProgressStyle.Point(i).apply {
                setColor(if (i < step) 0xFF6200EE.toInt() else 0xFFDDDDDD.toInt())
            }
        }
        val progressStyle = Notification.ProgressStyle()
            .setProgress(step)
            .setProgressSegments(segments)
            .setProgressPoints(points)

        val subtitle = if (isDelivered) "Sipariş teslim edildi"
        else "${status.label} · Tahmini varış: $eta"

        val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager

        return Notification.Builder(context, CHANNEL_ID)
            .setSmallIcon(Icon.createWithResource(context, context.applicationInfo.icon))
            .setContentTitle("Sipariş #$orderId")
            .setContentText(subtitle)
            .setStyle(progressStyle)
            .setOngoing(!isDelivered)
            .setOnlyAlertOnce(true)
            .setContentIntent(mainIntent(context, notificationId))
            .apply {
                if (!isDelivered && nm.canPostPromotedNotifications()) {
                    addExtras(Bundle().apply {
                        putBoolean("android.requestPromotedOngoing", true)
                    })
                }
                if (dismissalDate != null) {
                    val timeout = (dismissalDate * 1000L) - System.currentTimeMillis()
                    if (timeout > 0) setTimeoutAfter(timeout)
                }
            }
            .build()
    }

    // -------------------------------------------------------------------------
    // API < 36
    // -------------------------------------------------------------------------

    private fun buildLegacy(
        context: Context, orderId: String, status: DeliveryStatus,
        eta: String, notificationId: Int, dismissalDate: Long?
    ): Notification {
        val isDelivered = status == DeliveryStatus.DELIVERED

        val expandedView = RemoteViews(context.packageName, R.layout.notification_live_delivery).apply {
            setTextViewText(R.id.tvLiveDeliveryOrderId, "#$orderId")
            setTextViewText(R.id.tvLiveDeliveryStatus, status.label)
            setTextViewText(R.id.tvLiveDeliveryEta, eta)
            setProgressBar(R.id.progressLiveDelivery, 4, status.step, false)

            val dotIds = listOf(R.id.vStep1, R.id.vStep2, R.id.vStep3, R.id.vStep4)
            val labelIds = listOf(R.id.tvStep1, R.id.tvStep2, R.id.tvStep3, R.id.tvStep4)

            dotIds.forEachIndexed { index, dotId ->
                val s = index + 1
                val drawable = when {
                    s < status.step  -> R.drawable.den_live_dot_done
                    s == status.step -> R.drawable.den_live_dot_active
                    else             -> R.drawable.den_live_dot_inactive
                }
                setInt(dotId, "setBackgroundResource", drawable)
            }
            labelIds.forEachIndexed { index, labelId ->
                val color = if (index + 1 <= status.step)
                    Color.parseColor("#FF6200EE") else Color.parseColor("#FFAAAAAA")
                setTextColor(labelId, color)
            }

            setViewVisibility(R.id.pbLiveDeliverySpinner, if (isDelivered) View.GONE else View.VISIBLE)
            setViewVisibility(R.id.tvLiveDeliveryEtaLabel, if (isDelivered) View.GONE else View.VISIBLE)
            setViewVisibility(R.id.tvLiveDeliveryEta, if (isDelivered) View.GONE else View.VISIBLE)
        }

        return NotificationCompat.Builder(context, CHANNEL_ID)
            .setSmallIcon(R.mipmap.ic_launcher)
            .setContentTitle("Sipariş Takibi")
            .setContentText(status.label)
            .setCustomBigContentView(expandedView)
            .setStyle(NotificationCompat.DecoratedCustomViewStyle())
            .setOngoing(!isDelivered)
            .setOnlyAlertOnce(true)
            .setContentIntent(mainIntent(context, notificationId))
            .apply {
                if (dismissalDate != null) {
                    val timeout = (dismissalDate * 1000L) - System.currentTimeMillis()
                    if (timeout > 0) setTimeoutAfter(timeout)
                }
            }
            .build()
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    private fun ensureChannel(context: Context) {
        val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        if (nm.getNotificationChannel(CHANNEL_ID) != null) return
        val channel = NotificationChannel(CHANNEL_ID, "Teslimat", NotificationManager.IMPORTANCE_DEFAULT).apply {
            description = "Teslimat takibi bildirimleri"
            setSound(null, null)
        }
        nm.createNotificationChannel(channel)
    }

    private fun mainIntent(context: Context, requestCode: Int): PendingIntent {
        val intent = Intent(context, MainActivity::class.java)
        return PendingIntent.getActivity(context, requestCode, intent, PendingIntent.FLAG_IMMUTABLE)
    }

    private fun scheduleCancel(context: Context, notificationId: Int, dismissalDateSec: Long) {
        val delayMs = (dismissalDateSec * 1000L) - System.currentTimeMillis()
        if (delayMs <= 0) {
            cancel(context, notificationId)
        } else {
            Handler(Looper.getMainLooper()).postDelayed({ cancel(context, notificationId) }, delayMs)
        }
    }
}
