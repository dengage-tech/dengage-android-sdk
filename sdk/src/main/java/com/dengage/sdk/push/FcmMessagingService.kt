package com.dengage.sdk.push

import com.dengage.sdk.Dengage
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

        if (DengageUtils.showDengageNotification(remoteMessage.data)) {
            ContextHolder.resetContext(context = this)
            Dengage.onMessageReceived(remoteMessage.data)
        }
    }
}