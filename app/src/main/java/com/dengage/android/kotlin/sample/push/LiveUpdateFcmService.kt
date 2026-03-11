package com.dengage.android.kotlin.sample.push

import com.dengage.android.kotlin.sample.liveupdate.LiveUpdateManager
import com.dengage.sdk.push.FcmMessagingService
import com.google.firebase.messaging.RemoteMessage

class LiveUpdateFcmService : FcmMessagingService() {

    override fun onMessageReceived(remoteMessage: RemoteMessage) {
        val data = remoteMessage.data

        if (data["live_update_type"] != null) {
            handleLiveUpdate(data)
        } else {
            super.onMessageReceived(remoteMessage)
        }
    }

    private fun handleLiveUpdate(data: Map<String, String>) {
        when (data["live_update_type"]) {
            "delivery" -> {
                val statusName = data["delivery_status"] ?: return
                val status = runCatching {
                    LiveUpdateManager.DeliveryStatus.valueOf(statusName)
                }.getOrNull() ?: return

                LiveUpdateManager.showDeliveryUpdate(
                    context = applicationContext,
                    update = LiveUpdateManager.DeliveryUpdate(
                        orderId = data["order_id"] ?: "",
                        status = status,
                        estimatedTime = data["estimated_time"] ?: ""
                    )
                )
            }

            "sports" -> {
                LiveUpdateManager.showSportsUpdate(
                    context = applicationContext,
                    update = LiveUpdateManager.SportsUpdate(
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
                    "delivery" -> LiveUpdateManager.dismissDeliveryUpdate(applicationContext)
                    "sports"   -> LiveUpdateManager.dismissSportsUpdate(applicationContext)
                }
            }
        }
    }
}
