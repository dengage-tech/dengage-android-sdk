package com.dengage.sdk.push

import android.app.Activity
import android.content.Intent
import android.content.Intent.*
import android.net.Uri
import android.os.Bundle
import android.os.Handler
import android.os.Looper
import android.text.TextUtils
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.*
import com.dengage.sdk.util.extension.launchActivity
import com.dengage.sdk.util.extension.storeToPref

class NotificationNavigationDeciderActivity : Activity() {
    var launchActivity: Boolean = true
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        ContextHolder.resetContext(this.applicationContext)

    }

    override fun onRestart() {
        super.onRestart()
    }

    override fun onNewIntent(intent: Intent?) {
        super.onNewIntent(intent)
    }
    override fun onResume() {
        super.onResume()
        try {



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
                            sendingIntentObject.action = intent.action
                            sendingIntentObject.putExtras(extras)

                            sendingIntentObject.setPackage(packageName)

                        }


                        val message: Message? = Message.createFromIntent(extras)
                        message?.storeToPref()
                        val rawJson = extras.getString("RAW_DATA")
                        if(message?.messageDetails?.let { Prefs.getDynamicValue(it) } ==true){
                            launchDefaultActivity()
                          launchActivity=false
                        }
                        else message?.messageDetails?.let { Prefs.addDynamicValue(it,true) }


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

        }
        catch (ex:Exception)
        { ex.printStackTrace()

        }
        catch (ex: Throwable)
        { ex.printStackTrace()

        }
        catch (ex :IncompatibleClassChangeError)
        { ex.printStackTrace()

        }
        catch (ex :NoSuchFieldError)
        { ex.printStackTrace()

        }
        catch (ex :NoSuchMethodError)
        { ex.printStackTrace()

        }
        catch (ex:java.lang.AssertionError)
        {
            ex.printStackTrace()
        }
        catch (ex:AssertionError)
        {
            ex.printStackTrace()
        }
    }

    private fun killActivity() {

        Handler(Looper.getMainLooper()).postDelayed({
           // DengageUtils.unregisterBroadcast()
            if(Prefs.restartApplicationAfterPushClick == false)
                finish()
            else
                finishAffinity()
        }, 1200)
    }

    private fun startActivityLocal(intent :Intent)
    {
        if(launchActivity) {
            if (Prefs.restartApplicationAfterPushClick == false) {
                intent.flags = FLAG_ACTIVITY_CLEAR_TOP or FLAG_ACTIVITY_SINGLE_TOP or FLAG_ACTIVITY_NEW_TASK
            }
            startActivity(intent)
        }
    }

    private fun launchDefaultActivity()
    {
        launchActivity(null, null)
        /*if(Prefs.restartApplicationAfterPushClick == false)
            finish()
        else
            finishAffinity()*/
    }
}
