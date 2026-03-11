package com.dengage.android.kotlin.sample.liveupdate

import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.graphics.Color
import android.view.View
import android.widget.RemoteViews
import androidx.core.app.NotificationCompat
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.ui.activity.MainActivity

object LiveUpdateManager {

    private const val CHANNEL_ID = "den_live_update_channel"
    private const val CHANNEL_NAME = "Live Updates"
    const val NOTIFICATION_ID_DELIVERY = 2001
    const val NOTIFICATION_ID_SPORTS = 2002

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

    private fun ensureChannel(context: Context) {
        val manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        if (manager.getNotificationChannel(CHANNEL_ID) != null) return
        val channel = NotificationChannel(CHANNEL_ID, CHANNEL_NAME, NotificationManager.IMPORTANCE_DEFAULT).apply {
            description = "Dengage Live Update Notifications"
            setSound(null, null)
        }
        manager.createNotificationChannel(channel)
    }

    fun showDeliveryUpdate(context: Context, update: DeliveryUpdate) {
        ensureChannel(context)

        val expandedView = RemoteViews(context.packageName, R.layout.notification_live_delivery).apply {
            setTextViewText(R.id.tvLiveDeliveryOrderId, "#${update.orderId}")
            setTextViewText(R.id.tvLiveDeliveryStatus, update.status.label)
            setTextViewText(R.id.tvLiveDeliveryEta, update.estimatedTime)
            setProgressBar(R.id.progressLiveDelivery, 4, update.status.step, false)

            val dotIds = listOf(R.id.vStep1, R.id.vStep2, R.id.vStep3, R.id.vStep4)
            val labelIds = listOf(R.id.tvStep1, R.id.tvStep2, R.id.tvStep3, R.id.tvStep4)

            dotIds.forEachIndexed { index, dotId ->
                val stepNumber = index + 1
                val dotDrawable = when {
                    stepNumber < update.status.step  -> R.drawable.den_live_dot_done
                    stepNumber == update.status.step -> R.drawable.den_live_dot_active
                    else                             -> R.drawable.den_live_dot_inactive
                }
                setInt(dotId, "setBackgroundResource", dotDrawable)
            }

            labelIds.forEachIndexed { index, labelId ->
                val stepNumber = index + 1
                val textColor = when {
                    stepNumber <= update.status.step -> Color.parseColor("#FF6200EE")
                    else                             -> Color.parseColor("#FFAAAAAA")
                }
                setTextColor(labelId, textColor)
            }

            val isDelivered = update.status == DeliveryStatus.DELIVERED
            setViewVisibility(R.id.pbLiveDeliverySpinner, if (isDelivered) View.GONE else View.VISIBLE)
            setViewVisibility(R.id.tvLiveDeliveryEtaLabel, if (isDelivered) View.GONE else View.VISIBLE)
            setViewVisibility(R.id.tvLiveDeliveryEta, if (isDelivered) View.GONE else View.VISIBLE)
        }

        val pendingIntent = pendingIntentForMain(context, NOTIFICATION_ID_DELIVERY)

        val notification = NotificationCompat.Builder(context, CHANNEL_ID)
            .setSmallIcon(R.mipmap.ic_launcher)
            .setContentTitle("Sipariş Takibi")
            .setContentText(update.status.label)
            .setCustomBigContentView(expandedView)
            .setStyle(NotificationCompat.DecoratedCustomViewStyle())
            .setOngoing(update.status != DeliveryStatus.DELIVERED)
            .setOnlyAlertOnce(true)
            .setContentIntent(pendingIntent)
            .build()

        notify(context, NOTIFICATION_ID_DELIVERY, notification)
    }

    fun showSportsUpdate(context: Context, update: SportsUpdate) {
        ensureChannel(context)

        val expandedView = RemoteViews(context.packageName, R.layout.notification_live_sports).apply {
            setTextViewText(R.id.tvLiveSportsTeam1, update.team1)
            setTextViewText(R.id.tvLiveSportsTeam2, update.team2)
            setTextViewText(R.id.tvLiveSportsScore1, update.score1.toString())
            setTextViewText(R.id.tvLiveSportsScore2, update.score2.toString())
            setTextViewText(R.id.tvLiveSportsTime, update.matchTime)
            setTextViewText(R.id.tvLiveSportsPeriod, update.period)

            val isFinished = update.period.contains("Bitti", ignoreCase = true)
            val timeColor = if (isFinished) Color.parseColor("#FF888888") else Color.parseColor("#FFCC0000")
            setInt(R.id.tvLiveSportsTime, "setBackgroundColor", timeColor)
            // Hide the LIVE spinner when match is over
            setViewVisibility(R.id.pbLiveSportsIndicator, if (isFinished) View.GONE else View.VISIBLE)
        }

        val scoreText = "${update.team1} ${update.score1} - ${update.score2} ${update.team2}"
        val pendingIntent = pendingIntentForMain(context, NOTIFICATION_ID_SPORTS)

        val notification = NotificationCompat.Builder(context, CHANNEL_ID)
            .setSmallIcon(R.mipmap.ic_launcher)
            .setContentTitle("Canlı Skor")
            .setContentText(scoreText)
            .setCustomBigContentView(expandedView)
            .setStyle(NotificationCompat.DecoratedCustomViewStyle())
            .setOngoing(true)
            .setOnlyAlertOnce(true)
            .setContentIntent(pendingIntent)
            .build()

        notify(context, NOTIFICATION_ID_SPORTS, notification)
    }

    fun dismissDeliveryUpdate(context: Context) {
        cancel(context, NOTIFICATION_ID_DELIVERY)
    }

    fun dismissSportsUpdate(context: Context) {
        cancel(context, NOTIFICATION_ID_SPORTS)
    }

    private fun pendingIntentForMain(context: Context, requestCode: Int): PendingIntent {
        val intent = Intent(context, MainActivity::class.java)
        return PendingIntent.getActivity(context, requestCode, intent, PendingIntent.FLAG_IMMUTABLE)
    }

    private fun notify(context: Context, id: Int, notification: android.app.Notification) {
        val manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        manager.notify(id, notification)
    }

    private fun cancel(context: Context, id: Int) {
        val manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        manager.cancel(id)
    }
}
