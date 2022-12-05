package com.dengage.sdk.util

import android.app.ActivityManager
import android.content.Context
import android.content.pm.ApplicationInfo
import android.content.pm.PackageManager
import android.os.Build
import android.telephony.TelephonyManager
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.push.model.Message
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
        return "6.0.3.1"
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

    fun foregrounded(): Boolean {
        val appProcessInfo = ActivityManager.RunningAppProcessInfo()
        ActivityManager.getMyMemoryState(appProcessInfo)
        return appProcessInfo.importance == ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND || appProcessInfo.importance == ActivityManager.RunningAppProcessInfo.IMPORTANCE_VISIBLE
    }
}
