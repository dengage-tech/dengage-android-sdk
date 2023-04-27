package com.dengage.sdk.push

import com.dengage.sdk.Dengage
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageUtils
import com.huawei.hms.push.HmsMessageService
import com.huawei.hms.push.RemoteMessage

open class HmsMessagingService : HmsMessageService() {

    override fun onNewToken(token: String) {
        super.onNewToken(token)

        ContextHolder.resetContext(context = this)
        Dengage.onNewToken(token = token)
    }

    override fun onMessageReceived(remoteMessage: RemoteMessage) {
        super.onMessageReceived(remoteMessage)

        if (DengageUtils.showDengageNotification(remoteMessage.dataOfMap)) {
            ContextHolder.resetContext(context = this)
            Dengage.onMessageReceived(remoteMessage.dataOfMap)
        }
    }
}