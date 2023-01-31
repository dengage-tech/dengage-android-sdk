package com.dengage.sdk.push

import android.app.Activity
import android.content.Intent
import android.net.Uri
import android.os.Bundle
import android.os.Handler
import android.os.Looper
import android.text.TextUtils
import com.dengage.sdk.Dengage
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.*

class NotificationNavigationDeciderActivity : Activity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        ContextHolder.resetContext(this.applicationContext)

    }

    override fun onPause() {
        super.onPause()

        killActivity()


    }

    override fun onResume() {
        super.onResume()

        var sendingIntentObject: Intent

        if (intent != null) {

            val extras = intent.extras

            val uri: String?

            if (extras != null) {

                uri = extras.getString("targetUrl")


                if (uri != null && !TextUtils.isEmpty(uri)) {

                    sendingIntentObject = Intent(Intent.ACTION_VIEW, Uri.parse(uri))

                } else {

                    val packageName: String = packageName

                    sendingIntentObject =
                        Intent(this@NotificationNavigationDeciderActivity, getActivity())

                    sendingIntentObject.action=intent.action

                    sendingIntentObject.putExtras(extras)

                    sendingIntentObject.setPackage(packageName)

                }


            //    var message: Message? = Message.createFromIntent(extras)

                val rawJson = extras.getString("RAW_DATA")


                if (!TextUtils.isEmpty(rawJson)) {

                  //  message = GsonHolder.gson.fromJson(rawJson, Message::class.java)

                }

                DengageUtils.sendBroadCast(intent, this)
                try {
                    startActivity(sendingIntentObject)
                } catch (e: Exception) {
                    val packageName: String = packageName

                    sendingIntentObject =
                        Intent(this@NotificationNavigationDeciderActivity, getActivity())

                    sendingIntentObject.action=intent.action

                    sendingIntentObject.putExtras(extras)

                    sendingIntentObject.setPackage(packageName)
                    startActivity(sendingIntentObject)
                }

            } else {

                val packageName: String = packageName

                sendingIntentObject =
                    Intent(this@NotificationNavigationDeciderActivity, getActivity())

                sendingIntentObject.setPackage(packageName)
                startActivity(sendingIntentObject)

            }
            killActivity()
        }
    }

    private fun killActivity() {
        Handler(Looper.getMainLooper()).postDelayed({
            DengageUtils.unregisterBroadcast()
            onDestroy()
        }, 1200)
    }
}