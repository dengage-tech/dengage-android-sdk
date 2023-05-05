package com.dengage.sdk.util

import android.app.ActivityManager
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.content.pm.ApplicationInfo
import android.content.pm.PackageManager
import android.os.Build
import android.os.DeadObjectException
import android.telephony.TelephonyManager
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.push.NotificationReceiver
import java.util.*

object DengageUtils {

    fun getDeviceId(): String {
        if (Prefs.installationId == null) {
            Prefs.installationId = UUID.randomUUID().toString()
        }
        return Prefs.installationId!!
    }

    fun getCarrier(context: Context): String {
        return try {
            val manager = context.getSystemService(Context.TELEPHONY_SERVICE) as TelephonyManager?
            manager?.networkOperator ?: ""
        } catch (e: Exception) {
            DengageLogger.error(e.message)
            ""
        }
        catch (e:Throwable)
        {
            ""
        }
    }

    fun getAppVersion(context: Context): String? {
        return try {
            val pInfo = context.packageManager.getPackageInfo(context.packageName, 0)
            pInfo.versionName
        } catch (e: Exception) {
            DengageLogger.error(e.message)
            null
        }
        catch (e:Throwable)
        {
            null
        }
    }

    fun getSdkVersion(): String {
        return "6.0.30.3"
    }

    fun getUserAgent(context: Context): String {
        try {
            val appLabel = "${getAppLabel(context, "An Android App")}/" +
                    "${getAppVersion(context)} ${Build.MANUFACTURER}/${Build.MODEL} " +
                    "${System.getProperty("http.agent")} Mobile/${Build.ID}"

            return appLabel.replace("[^\\x00-\\x7F]".toRegex(), "")
        }
        catch (e:Exception)
        {

        }
        catch (e:Throwable)
        {

        }
        return ""
    }

    private fun getAppLabel(context: Context, defaultText: String?): String? {
        val lPackageManager = context.packageManager
        var lApplicationInfo: ApplicationInfo? = null
        try {
            lApplicationInfo =
                lPackageManager.getApplicationInfo(context.applicationInfo.packageName, 0)
        } catch (e: Exception) {

        }
        catch (ex: Throwable)
        { ex.printStackTrace()

        }
        catch (e: DeadObjectException)
        {

        }
        return (if (lApplicationInfo != null) lPackageManager.getApplicationLabel(lApplicationInfo) else defaultText) as String?
    }

    fun getMetaData(
        context: Context? = null,
        name: String
    ): String? {
        return try {
            val applicationInfo = (context
                ?: ContextHolder.context).packageManager.getApplicationInfo(
                ContextHolder.context.packageName,
                PackageManager.GET_META_DATA
            )
            val bundle = applicationInfo.metaData
            bundle.getString(name)
        } catch (e: Exception) {
            null
        }
        catch (ex: Throwable)
        { ex.printStackTrace()
            null
        }
        catch (e: DeadObjectException)
        {
            e.printStackTrace()
            null
        }
    }

    fun generateUUID(): String {
        return UUID.randomUUID().toString()
    }

    fun showDengageNotification(data: Map<String, String>): Boolean {
        try {
            val message = Message.createFromMap(data)
            return Constants.MESSAGE_SOURCE == message.messageSource
        }
        catch (e: Exception) {
            e.printStackTrace()
        }
        catch (ex: Throwable)
        { ex.printStackTrace()

        }
        return false
    }


    fun getIANAFormatTimeZone(): String {
        try {
            return TimeZone.getDefault().id
        } catch (e: Exception) {
            e.printStackTrace()
        }
        catch (ex: Throwable)
        { ex.printStackTrace()

        }
        return ""
    }

    fun isAppInForeground(): Boolean {
        return try {
            val appProcessInfo = ActivityManager.RunningAppProcessInfo()
            ActivityManager.getMyMemoryState(appProcessInfo)
            appProcessInfo.importance == ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND || appProcessInfo.importance == ActivityManager.RunningAppProcessInfo.IMPORTANCE_VISIBLE
        } catch (e: Exception) {

            return false
        }
        catch (ex: Throwable)
        { ex.printStackTrace()
            return false
        }
        catch (e: DeadObjectException)
        {
            return false
        }
    }


    fun registerBroadcast() {
        try {
            val filter = IntentFilter(Constants.PUSH_RECEIVE_EVENT)
            filter.addAction(Constants.PUSH_OPEN_EVENT)
            filter.addAction(Constants.PUSH_DELETE_EVENT)
            filter.addAction(Constants.PUSH_ACTION_CLICK_EVENT)
            filter.addAction(Constants.PUSH_ITEM_CLICK_EVENT)
            filter.addAction("com.dengage.push.intent.CAROUSEL_ITEM_CLICK")
            ContextHolder.context.applicationContext.registerReceiver(
                NotificationReceiver(),
                filter
            )
        } catch (e: Exception) {
          //  e.printStackTrace()
        }
        catch (ex: Throwable)
        { ex.printStackTrace()

        }
    }

    fun unregisterBroadcast() {
        try {
            ContextHolder.context.unregisterReceiver(NotificationReceiver())
        } catch (e: Exception) {

        }
        catch (ex: Throwable)
        { ex.printStackTrace()

        }
    }

    fun sendBroadCast(intent: Intent, context: Context) {
        try {
            val broadCastIntent = Intent(intent.action)
            broadCastIntent.putExtras(intent.extras!!)
            context.sendBroadcast(broadCastIntent)
        } catch (e: Exception) {

        }
        catch (ex: Throwable)
        { ex.printStackTrace()

        }
    }


    fun isDeeplink(targetUrl: String): Boolean {
        return targetUrl.startsWith(Prefs.inAppDeeplink,ignoreCase = true) &&targetUrl.isNotEmpty()
    }

   /* fun getSdkDefaultObj():SdkParameters
    {
        return SdkParameters(
            appId="",
            accountId=0,
            accountName="",
            eventsEnabled=false,
            inboxEnabled=false,
            inAppEnabled = false,
            subscriptionEnabled = false,
            inAppFetchIntervalInMin = 0,
            expiredMessagesFetchIntervalInMin = 0,
            inAppMinSecBetweenMessages = 0,
            lastFetchTimeInMillis = 0,
            appTrackingEnabled = false,
            appTrackingList = ArrayList(),
            realTimeInAppEnabled = false,
            realTimeInAppFetchIntervalInMinutes = 0,
            realTimeInAppSessionTimeoutMinutes = 0
        )
    }

    fun getSubscriptionDefaultObj() : Subscription
    {
        return Subscription()
    }*/

    fun restartApplication(): Boolean {
        var restartApplication = false
        try {
            if (Prefs.restartApplicationAfterPushClick == true) {
                restartApplication = true
            }
            if (!restartApplication) {
                restartApplication = Dengage.getCurrentActivity() == null
            }

        } catch (e: Exception) {

            restartApplication = true
        }

        return restartApplication
    }
}
