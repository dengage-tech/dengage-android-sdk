package com.dengage.android.kotlin.sample.liveupdate

import android.app.Notification
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.graphics.Color
import android.graphics.drawable.Icon
import android.os.Build
import android.os.Bundle
import android.util.Log
import android.view.View
import android.widget.RemoteViews
import androidx.annotation.RequiresApi
import androidx.core.app.NotificationCompat
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.ui.activity.MainActivity
import com.dengage.sdk.liveupdate.LiveUpdateHandler
import com.dengage.sdk.liveupdate.LiveUpdatePayload

class DeliveryLiveUpdateHandler : LiveUpdateHandler {

    override val channelId = "den_live_update_delivery_v2"
    override val channelName = "Teslimat"
    override val channelDescription = "Teslimat takibi bildirimleri"

    enum class DeliveryStatus(val label: String, val step: Int) {
        ORDER_RECEIVED("Sipariş Alındı", 1),
        PREPARING("Hazırlanıyor", 2),
        ON_THE_WAY("Yolda", 3),
        DELIVERED("Teslim Edildi", 4)
    }

    override fun buildNotification(context: Context, payload: LiveUpdatePayload): Notification? {
        val cs = payload.contentState
        val statusName = cs["delivery_status"] ?: return null
        val status = runCatching { DeliveryStatus.valueOf(statusName) }.getOrNull() ?: return null
        val orderId = cs["order_id"] ?: ""
        val eta = cs["estimated_time"] ?: ""

        val notificationId = payload.activityId.hashCode()

        return if (Build.VERSION.SDK_INT >= 36) {
            buildApi36(context, orderId, status, eta, notificationId)
        } else {
            buildLegacy(context, orderId, status, eta, notificationId)
        }
    }

    // -------------------------------------------------------------------------
    // API 36+
    // -------------------------------------------------------------------------

    @RequiresApi(36)
    private fun buildApi36(
        context: Context, orderId: String, status: DeliveryStatus,
        eta: String, notificationId: Int
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
        val canPost = nm.canPostPromotedNotifications()
        val permState = context.checkSelfPermission("android.permission.POST_PROMOTED_NOTIFICATIONS")
        Log.d(
            "LiveUpdateDiag",
            "Delivery: SDK_INT=${Build.VERSION.SDK_INT}, canPostPromoted=$canPost, " +
                    "POST_PROMOTED_NOTIFICATIONS=$permState (0=granted,-1=denied), " +
                    "isDelivered=$isDelivered"
        )

        return Notification.Builder(context, channelId)
            .setSmallIcon(Icon.createWithResource(context, context.applicationInfo.icon))
            .setContentTitle("Sipariş #$orderId")
            .setContentText(subtitle)
            .setStyle(progressStyle)
            .setOngoing(!isDelivered)
            .setOnlyAlertOnce(true)
            .setVisibility(Notification.VISIBILITY_PUBLIC)
            .setCategory(Notification.CATEGORY_PROGRESS)
            .setContentIntent(mainIntent(context, notificationId))
            .setShortCriticalText(if (isDelivered) "Teslim edildi" else status.label)
            .apply {
                if (!isDelivered && nm.canPostPromotedNotifications()) {
                    addExtras(Bundle().apply {
                        putBoolean("android.requestPromotedOngoing", true)
                    })
                }
            }
            .build()
    }

    // -------------------------------------------------------------------------
    // API < 36
    // -------------------------------------------------------------------------

    private fun buildLegacy(
        context: Context, orderId: String, status: DeliveryStatus,
        eta: String, notificationId: Int
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

        return NotificationCompat.Builder(context, channelId)
            .setSmallIcon(R.mipmap.ic_launcher)
            .setContentTitle("Sipariş Takibi")
            .setContentText(status.label)
            .setCustomBigContentView(expandedView)
            .setStyle(NotificationCompat.DecoratedCustomViewStyle())
            .setOngoing(!isDelivered)
            .setOnlyAlertOnce(true)
            .setVisibility(NotificationCompat.VISIBILITY_PUBLIC)
            .setCategory(NotificationCompat.CATEGORY_PROGRESS)
            .setContentIntent(mainIntent(context, notificationId))
            .build()
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    private fun mainIntent(context: Context, requestCode: Int): PendingIntent {
        val intent = Intent(context, MainActivity::class.java)
        return PendingIntent.getActivity(context, requestCode, intent, PendingIntent.FLAG_IMMUTABLE)
    }
}
