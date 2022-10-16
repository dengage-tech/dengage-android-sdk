package com.dengage.sdk.push

import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.text.TextUtils
import androidx.appcompat.app.AppCompatActivity
import com.dengage.sdk.Dengage
import com.dengage.sdk.R
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.*
import com.dengage.sdk.util.extension.toJson

class NotificationNavigationDeciderActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_notification_navigation_decider)
        if (intent != null) {

            val extras = intent.extras

            val uri: String?

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

                sendBroadcast(message.toJson(),extras)


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

    fun sendBroadcast(json: String,jsonDataBundle:Bundle) {
        DengageLogger.verbose("sendBroadcast method is called")
        try {
            val intent = Intent(Constants.PUSH_OPEN_EVENT)
            intent.putExtra("RAW_DATA", json)
            DengageLogger.verbose("RAW_DATA: $json")
            intent.putExtras(jsonDataBundle)
            intent.setPackage(packageName)
            sendBroadcast(intent)
        } catch (e: java.lang.Exception) {
            DengageLogger.error("sendBroadcast: " + e.message)
        }
    }
}