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

class SportsLiveUpdateHandler : LiveUpdateHandler {

    companion object {
        const val CHANNEL_ID = "den_live_update_sports"
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
        val team1 = cs["team1"] ?: ""
        val team2 = cs["team2"] ?: ""
        val score1 = cs["score1"]?.toIntOrNull() ?: 0
        val score2 = cs["score2"]?.toIntOrNull() ?: 0
        val matchTime = cs["match_time"] ?: ""
        val period = cs["period"] ?: ""

        ensureChannel(context)

        val notification = if (Build.VERSION.SDK_INT >= 36) {
            buildApi36(context, team1, team2, score1, score2, matchTime, period, notificationId, dismissal)
        } else {
            buildLegacy(context, team1, team2, score1, score2, matchTime, period, notificationId, dismissal)
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
        context: Context,
        team1: String, team2: String,
        score1: Int, score2: Int,
        matchTime: String, period: String,
        notificationId: Int, dismissalDate: Long?
    ): Notification {
        val isFinished = period.contains("Bitti", ignoreCase = true)
        val scoreText = "$team1  $score1 - $score2  $team2"
        val statusText = "$matchTime · $period"

        val nm = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager

        return Notification.Builder(context, CHANNEL_ID)
            .setSmallIcon(Icon.createWithResource(context, context.applicationInfo.icon))
            .setContentTitle(scoreText)
            .setContentText(statusText)
            .setStyle(Notification.BigTextStyle().bigText("$scoreText\n$statusText"))
            .setOngoing(!isFinished)
            .setOnlyAlertOnce(true)
            .setContentIntent(mainIntent(context, notificationId))
            .apply {
                if (!isFinished && nm.canPostPromotedNotifications()) {
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
        context: Context,
        team1: String, team2: String,
        score1: Int, score2: Int,
        matchTime: String, period: String,
        notificationId: Int, dismissalDate: Long?
    ): Notification {
        val isFinished = period.contains("Bitti", ignoreCase = true)

        val expandedView = RemoteViews(context.packageName, R.layout.notification_live_sports).apply {
            setTextViewText(R.id.tvLiveSportsTeam1, team1)
            setTextViewText(R.id.tvLiveSportsTeam2, team2)
            setTextViewText(R.id.tvLiveSportsScore1, score1.toString())
            setTextViewText(R.id.tvLiveSportsScore2, score2.toString())
            setTextViewText(R.id.tvLiveSportsTime, matchTime)
            setTextViewText(R.id.tvLiveSportsPeriod, period)

            setInt(R.id.tvLiveSportsTime, "setBackgroundColor",
                if (isFinished) Color.parseColor("#FF888888") else Color.parseColor("#FFCC0000"))
            setViewVisibility(R.id.pbLiveSportsIndicator, if (isFinished) View.GONE else View.VISIBLE)
        }

        val scoreText = "$team1 $score1 - $score2 $team2"
        return NotificationCompat.Builder(context, CHANNEL_ID)
            .setSmallIcon(R.mipmap.ic_launcher)
            .setContentTitle("Canlı Skor")
            .setContentText(scoreText)
            .setCustomBigContentView(expandedView)
            .setStyle(NotificationCompat.DecoratedCustomViewStyle())
            .setOngoing(!isFinished)
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
        val channel = NotificationChannel(CHANNEL_ID, "Canlı Skor", NotificationManager.IMPORTANCE_DEFAULT).apply {
            description = "Canlı maç skoru bildirimleri"
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
