package com.dengage.hms

import com.dengage.sdk.Dengage
import com.dengage.sdk.push.GeofenceSilentPushDispatcher
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageUtils
import com.huawei.hms.push.HmsMessageService
import com.huawei.hms.push.RemoteMessage

open class HmsMessagingService : HmsMessageService() {

    override fun onNewToken(token: String) {
        super.onNewToken(token)

        ContextHolder.resetContext(this)
        Dengage.onNewToken(token = token)
    }

    override fun onMessageReceived(remoteMessage: RemoteMessage) {
        super.onMessageReceived(remoteMessage)

        // Silent push: sourceType == geofence ise fence'leri sunucudan yeniden çek (resync)
        if (remoteMessage.dataOfMap["sourceType"].equals("geofence", ignoreCase = true)) {
            ContextHolder.resetContext(this)
            GeofenceSilentPushDispatcher.dispatch(applicationContext, remoteMessage.dataOfMap)
            return
        }

        if (DengageUtils.showDengageNotification(remoteMessage.dataOfMap)) {
            ContextHolder.resetContext(this)
            Dengage.onMessageReceived(remoteMessage.dataOfMap)
        }
    }
}