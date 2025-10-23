package com.dengage.sdk.push

import android.app.Activity
import android.app.NotificationManager
import android.content.Context
import android.content.Intent
import android.content.Intent.FLAG_ACTIVITY_CLEAR_TOP
import android.content.Intent.FLAG_ACTIVITY_SINGLE_TOP
import android.net.Uri
import android.os.Bundle
import android.os.Handler
import android.os.Looper
import android.text.TextUtils
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.*
import com.dengage.sdk.util.extension.launchActivity
import com.dengage.sdk.util.extension.shouldProcessPush
import com.dengage.sdk.util.extension.storeToPref

class NotificationNavigationDeciderActivity : Activity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        val id = intent.extras?.getString("id", "")
        if ("NO".equals(id, ignoreCase = true)) {
            val requestCode = intent.extras?.getInt("requestCode")
            if (!Constants.listOfNotificationIds.contains(requestCode)) {
                Constants.listOfNotificationIds.add(requestCode)
            }
            val message = Message.createFromIntent(intent.extras!!)
            clearNotification(message)
            finish()
            return
        }

        ContextHolder.resetContext(this.applicationContext)
    }


    override fun onResume() {
        super.onResume()
        try {
            DengageUtils.registerBroadcast()
            if (!Constants.listOfNotificationIds.contains(intent.extras?.getInt("requestCode"))) {
                var sendingIntentObject: Intent
                if (intent != null) {
                    Constants.listOfNotificationIds.add(intent?.extras?.getInt("requestCode"))

                    val extras = intent.extras

                    val uri: String?
                    val id: String?

                    if (extras != null) {
                        val message: Message? = Message.createFromIntent(extras)
                        if(message?.shouldProcessPush()==false)return
                        var targetUrl: String? = ""
                        var carouselId: String?
                        if (message?.carouselContent.isNullOrEmpty()||!message?.source.isNullOrBlank()) {
                            targetUrl= message?.targetUrl

                        } else {

                            targetUrl = message?.current?.let { message.carouselContent?.get(it)?.targetUrl }
                            carouselId = message?.current?.let { message.carouselContent?.get(it)?.id }
                            if(!carouselId.isNullOrEmpty())
                            {
                                intent.putExtra("carouselId", carouselId)
                            }
                            if(targetUrl.isNullOrEmpty())
                            {
                                targetUrl=message?.targetUrl
                            }
                        }
                        uri = targetUrl


                        if (uri != null && !TextUtils.isEmpty(uri) && Prefs.disableOpenWebUrl ==false) {
                            sendingIntentObject = Intent(Intent.ACTION_VIEW, Uri.parse(uri))

                        } else {

                            val packageName: String = packageName

                            sendingIntentObject =
                                Intent(this@NotificationNavigationDeciderActivity, getActivity())
                            sendingIntentObject.action = intent.action
                            sendingIntentObject.putExtras(extras)

                            sendingIntentObject.setPackage(packageName)

                        }


                        message?.storeToPref()
                        val rawJson = extras.getString("RAW_DATA")


                        if (!TextUtils.isEmpty(rawJson)) {
                            //  message = GsonHolder.gson.fromJson(rawJson, Message::class.java)
                        }

                        DengageUtils.sendBroadCast(intent, this)

                        try {
                            startActivityLocal(sendingIntentObject)
                        } catch (e: Exception) {
                            val packageName: String = packageName
                            sendingIntentObject =
                                Intent(this@NotificationNavigationDeciderActivity, getActivity())
                            sendingIntentObject.putExtras(extras)
                            sendingIntentObject.action = intent.action
                            sendingIntentObject.setPackage(packageName)
                            startActivityLocal(sendingIntentObject)
                        }

                    } else {

                        val packageName: String = packageName

                        sendingIntentObject =
                            Intent(this@NotificationNavigationDeciderActivity, getActivity())

                        sendingIntentObject.setPackage(packageName)
                        startActivityLocal(sendingIntentObject)

                    }
                    killActivity()
                }
            } else {
                if (DengageUtils.isAppInForeground()) {
                    launchActivity(null, null)
                    if (Prefs.restartApplicationAfterPushClick == false)
                        finish()
                    else
                        finishAffinity()
                }

            }
            clearNotification()
        } catch (ex: Exception) {
            ex.printStackTrace()

        } catch (ex: Throwable) {
            ex.printStackTrace()

        } catch (ex: IncompatibleClassChangeError) {
            ex.printStackTrace()

        } catch (ex: NoSuchFieldError) {
            ex.printStackTrace()

        } catch (ex: NoSuchMethodError) {
            ex.printStackTrace()

        } catch (ex: java.lang.AssertionError) {
            ex.printStackTrace()
        } catch (ex: AssertionError) {
            ex.printStackTrace()
        }
    }

    private fun killActivity() {
        Constants.isActivityPerformed = true
        Handler(Looper.getMainLooper()).postDelayed({
            DengageUtils.unregisterBroadcast()
            if (Prefs.restartApplicationAfterPushClick == false)
                finish()
            else
                finishAffinity()
        }, 1200)
    }

    private fun startActivityLocal(intent: Intent) {
        if (Prefs.restartApplicationAfterPushClick == false) {
            intent.flags = FLAG_ACTIVITY_CLEAR_TOP or FLAG_ACTIVITY_SINGLE_TOP
        }
        startActivity(intent)
    }

    private fun clearNotification()
    {
        if (intent != null) {
            val extras = intent.extras
            if (extras != null) {
                val message: Message = Message.createFromIntent(extras)
                this.clearNotification(message)
            }
        }
    }
}