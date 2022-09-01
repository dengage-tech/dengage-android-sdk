package com.dengage.sdk.push

import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.text.TextUtils
import androidx.appcompat.app.AppCompatActivity
import com.dengage.sdk.Dengage
import com.dengage.sdk.R
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.GsonHolder

class NotificationNavigationDeciderActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_notification_navigation_decider)
        ContextHolder.context = this
        if (intent != null) {
            val extras = intent.extras
            var uri: String? = null
            if (extras != null) {
                uri = extras.getString("targetUrl")

                if (uri != null && !TextUtils.isEmpty(uri)) {
                    intent = Intent(Intent.ACTION_VIEW, Uri.parse(uri));
                } else {
                    val packageName: String = packageName

                    intent = Intent(this@NotificationNavigationDeciderActivity, getActivity())
                    intent.putExtras(extras)
                    intent.setPackage(packageName)
                }
                var message: Message? = Message.createFromIntent(extras)
                val rawJson = extras.getString("RAW_DATA")

                if (!TextUtils.isEmpty(rawJson)) {
                    message = GsonHolder.gson.fromJson(rawJson, Message::class.java)
                }
                callOpenEvent(message)
                clearNotification(message)
            } else {
                val packageName: String = packageName

                intent = Intent(this@NotificationNavigationDeciderActivity, getActivity())
                intent.setPackage(packageName)

            }

            startActivity(intent)
            finishAffinity()
        }
    }


    private fun callOpenEvent(message: Message?) {
        Dengage.sendOpenEvent("", "", message)

    }
}