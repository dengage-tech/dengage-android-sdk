package com.dengage.sdk.util

import android.app.ActivityManager
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.content.pm.ApplicationInfo
import android.content.pm.PackageManager
import android.os.Build
import android.telephony.TelephonyManager
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.push.model.Message
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
    }

    fun getAppVersion(context: Context): String? {
        return try {
            val pInfo = context.packageManager.getPackageInfo(context.packageName, 0)
            pInfo.versionName
        } catch (e: Exception) {
            DengageLogger.error(e.message)
            null
        }
    }

    fun getSdkVersion(): String {
        return "6.0.14.3"
    }

    fun getUserAgent(context: Context): String {
        val appLabel = "${getAppLabel(context, "An Android App")}/" +
                "${getAppVersion(context)} ${Build.MANUFACTURER}/${Build.MODEL} " +
                "${System.getProperty("http.agent")} Mobile/${Build.ID}"

        return appLabel.replace("[^\\x00-\\x7F]".toRegex(), "")
    }

    private fun getAppLabel(context: Context, defaultText: String?): String? {
        val lPackageManager = context.packageManager
        var lApplicationInfo: ApplicationInfo? = null
        try {
            lApplicationInfo =
                lPackageManager.getApplicationInfo(context.applicationInfo.packageName, 0)
        } catch (e: PackageManager.NameNotFoundException) {
            DengageLogger.error(e.message)
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
        } catch (e: PackageManager.NameNotFoundException) {
            null
        }
    }

    fun generateUUID(): String {
        return UUID.randomUUID().toString()
    }

    fun showDengageNotification(data: Map<String, String>): Boolean {
        val message = Message.createFromMap(data)
        return Constants.MESSAGE_SOURCE == message.messageSource
    }


    fun getIANAFormatTimeZone(): String {
        try {
            return TimeZone.getDefault().id
        } catch (e: Exception) {
            e.printStackTrace()
        }
        return ""
    }

    fun isAppInForeground(): Boolean {
        return try {
            val appProcessInfo = ActivityManager.RunningAppProcessInfo()
            ActivityManager.getMyMemoryState(appProcessInfo)
            appProcessInfo.importance == ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND || appProcessInfo.importance == ActivityManager.RunningAppProcessInfo.IMPORTANCE_VISIBLE
        } catch (e: Exception) {
            e.printStackTrace()
            true
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
           // e.printStackTrace()
        }
    }

    fun unregisterBroadcast() {
        try {
            ContextHolder.context.unregisterReceiver(NotificationReceiver())
        } catch (e: Exception) {
           // e.printStackTrace()
        }
    }

    fun sendBroadCast(intent: Intent, context: Context) {
        try {
            val broadCastIntent = Intent(intent.action)
            broadCastIntent.putExtras(intent.extras!!)
            context.sendBroadcast(broadCastIntent)
        } catch (e: Exception) {
           // e.printStackTrace()
        }
    }

    fun isDeeplink(targetUrl: String): Boolean {
        return targetUrl.startsWith(Prefs.inAppDeeplink,ignoreCase = true) &&targetUrl.isNotEmpty()
    }
}