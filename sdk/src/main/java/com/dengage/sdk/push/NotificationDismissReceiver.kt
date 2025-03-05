package com.dengage.sdk.push

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageUtils

class NotificationDismissReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        val extras = intent.extras
        val requestCode = intent.extras?.getInt("requestCode")
        if (!Constants.listOfNotificationIds.contains(requestCode)) {
            Constants.listOfNotificationIds.add(requestCode)
        }
        DengageUtils.registerBroadcast()
        DengageUtils.sendBroadCast(intent, context)
        if (extras != null) {
            val message = Message.createFromIntent(extras)
            context.clearNotification(message)
        }
    }
}
