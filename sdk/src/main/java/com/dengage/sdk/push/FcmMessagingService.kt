package com.dengage.sdk.push

import com.dengage.sdk.Dengage
import com.dengage.sdk.liveupdate.DengageLiveUpdateManager
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageUtils
import com.google.firebase.messaging.FirebaseMessagingService
import com.google.firebase.messaging.RemoteMessage

open class FcmMessagingService : FirebaseMessagingService() {

    override fun onNewToken(token: String) {
        super.onNewToken(token)

        ContextHolder.resetContext(context = this)
        Dengage.onNewToken(token = token)
    }

    override fun onMessageReceived(remoteMessage: RemoteMessage) {
        super.onMessageReceived(remoteMessage)

        val data = remoteMessage.data

        if (data.containsKey("live_notification")) {
            ContextHolder.resetContext(context = this)
            DengageLiveUpdateManager.handleFromFcmData(applicationContext, data)
            return
        }

        if (DengageUtils.showDengageNotification(data)) {
            ContextHolder.resetContext(context = this)
            Dengage.onMessageReceived(data)
        }
    }
}
