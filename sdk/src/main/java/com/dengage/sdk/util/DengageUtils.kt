package com.dengage.sdk.util

import android.Manifest
import android.app.ActivityManager
import android.app.NotificationManager
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.content.pm.ApplicationInfo
import android.content.pm.PackageManager
import android.os.Build
import android.os.DeadObjectException
import android.telephony.TelephonyManager
import androidx.core.content.ContextCompat
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.data.remote.api.NotificationDisplayPriorityConfiguration
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.inapp.InAppBroadcastReceiver
import com.dengage.sdk.push.NRTrampoline
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
        } catch (_: Throwable) {
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
        } catch (_: Throwable) {
            null
        }
    }

    fun getSdkVersion(): String {
        return "6.0.100"
    }

    fun getUserAgent(context: Context): String {
        try {
            val appLabel = "${getAppLabel(context, "An Android App")}/" +
                    "${getAppVersion(context)} ${Build.MANUFACTURER}/${Build.MODEL} " +
                    "${System.getProperty("http.agent")} Mobile/${Build.ID}"

            return appLabel.replace("[^\\x00-\\x7F]".toRegex(), "")
        } catch (_: Exception) {

        } catch (_: Throwable) {

        }
        return ""
    }

    private fun getAppLabel(context: Context, defaultText: String?): String? {
        val lPackageManager = context.packageManager
        var lApplicationInfo: ApplicationInfo? = null
        try {
            lApplicationInfo =
                lPackageManager.getApplicationInfo(context.applicationInfo.packageName, 0)
        } catch (_: Exception) {

        } catch (ex: Throwable) {
            ex.printStackTrace()

        } catch (_: DeadObjectException) {

        }
        return (if (lApplicationInfo != null) lPackageManager.getApplicationLabel(lApplicationInfo) else defaultText) as String?
    }

    fun getMetaData(
        context: Context? = null,
        name: String,
    ): String? {
        return try {
            val applicationInfo = (context
                ?: ContextHolder.context).packageManager.getApplicationInfo(
                ContextHolder.context.packageName,
                PackageManager.GET_META_DATA
            )
            val bundle = applicationInfo.metaData
            bundle.getString(name)
        } catch (_: Exception) {
            null
        } catch (ex: Throwable) {
            ex.printStackTrace()
            null
        } catch (e: DeadObjectException) {
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
        } catch (e: Exception) {
            e.printStackTrace()
        } catch (ex: Throwable) {
            ex.printStackTrace()

        }
        return false
    }


    fun getIANAFormatTimeZone(): String {
        try {
            return TimeZone.getDefault().id
        } catch (e: Exception) {
            e.printStackTrace()
        } catch (ex: Throwable) {
            ex.printStackTrace()

        }
        return ""
    }

    fun isAppInForeground(): Boolean {
        return try {
            val appProcessInfo = ActivityManager.RunningAppProcessInfo()
            ActivityManager.getMyMemoryState(appProcessInfo)
            appProcessInfo.importance == ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND || appProcessInfo.importance == ActivityManager.RunningAppProcessInfo.IMPORTANCE_VISIBLE
        } catch (_: Exception) {

            return false
        } catch (ex: Throwable) {
            ex.printStackTrace()
            return false
        } catch (_: DeadObjectException) {
            return false
        }
    }


    private val pushBroadcastActions = listOf(
        Constants.PUSH_RECEIVE_EVENT,
        Constants.PUSH_OPEN_EVENT,
        Constants.PUSH_DELETE_EVENT,
        Constants.PUSH_ACTION_CLICK_EVENT,
        Constants.PUSH_ITEM_CLICK_EVENT,
        "com.dengage.push.intent.CAROUSEL_ITEM_CLICK"
    )

    /**
     * Registers the SDK's internal push receiver ([NRTrampoline]) at most once per process.
     *
     * Previously every call registered a brand-new NRTrampoline instance and nothing ever
     * unregistered it, so after N pushes there were N receivers, each calling notify() with the
     * same id. On the phone the same id just replaces the notification, but companion apps that
     * mirror notifications to a watch (e.g. Huawei Health) forward every notify() call, which
     * produced duplicate notifications on the watch.
     *
     * PUSH_RECEIVE_EVENT is the only action that is broadcast with setPackage() (see
     * [com.dengage.sdk.Dengage.sendBroadcast]), so it is the only one that also reaches a
     * manifest-declared receiver in the host app. When the app has such a receiver (see README,
     * "Defining Custom Receiver") the internal receiver does not subscribe to PUSH_RECEIVE_EVENT,
     * so each push is rendered exactly once.
     *
     * The click/dismiss actions are re-broadcast implicitly by [sendBroadCast] (no setPackage), and
     * implicit broadcasts never reach manifest receivers on Android 8+. The internal receiver must
     * therefore always subscribe to those, regardless of what the manifest declares.
     */
    @Synchronized
    fun registerBroadcast() {
        if (Constants.isBCRegistered) return
        try {
            val context = ContextHolder.context.applicationContext
            val filter = IntentFilter()
            for (action in pushBroadcastActions) {
                if (action == Constants.PUSH_RECEIVE_EVENT && hasManifestReceiver(context, action)) {
                    DengageLogger.verbose("registerBroadcast: $action is handled by the app's manifest receiver, skipping internal receiver")
                } else {
                    filter.addAction(action)
                }
            }
            if (filter.countActions() > 0) {
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                    context.registerReceiver(NRTrampoline(), filter, Context.RECEIVER_EXPORTED)
                } else {
                    context.registerReceiver(NRTrampoline(), filter)
                }
            }
            Constants.isBCRegistered = true
        } catch (_: Exception) {
            //  e.printStackTrace()
        } catch (ex: Throwable) {
            ex.printStackTrace()

        }
    }

    private fun hasManifestReceiver(context: Context, action: String): Boolean {
        return try {
            val intent = Intent(action).setPackage(context.packageName)
            context.packageManager.queryBroadcastReceivers(intent, 0).any { it.activityInfo != null }
        } catch (_: Throwable) {
            false
        }
    }

    private val postedNotificationIds = mutableSetOf<Int>()

    /**
     * Returns true the first time a notification id is posted in this process and false for every
     * later call with the same id. Callers skip notify() when this returns false so a push is never
     * posted twice, even if more than one receiver handled the same broadcast.
     */
    @Synchronized
    fun markNotificationPosted(notificationId: Int): Boolean {
        return postedNotificationIds.add(notificationId)
    }

    fun unregisterBroadcast() {
        try {

            //ContextHolder.context.applicationContext.unregisterReceiver(NRTrampoline())

        } catch (_: Exception) {
            //e.printStackTrace()
        } catch (_: Throwable) {
            //  ex.printStackTrace()

        }
    }

    fun registerInAppBroadcast() {
        try {
            val intentFilter = IntentFilter(Constants.DEEPLINK_RETRIEVE_EVENT)
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                ContextHolder.context.applicationContext.registerReceiver(
                    InAppBroadcastReceiver(),
                    intentFilter, Context.RECEIVER_EXPORTED
                )
            } else {
                ContextHolder.context.applicationContext.registerReceiver(
                    InAppBroadcastReceiver(),
                    intentFilter
                )
            }


        } catch (_: Exception) {
            //  e.printStackTrace()
        } catch (_: Throwable) {
            //  ex.printStackTrace()

        }
    }

    fun unregisterInAppBroadcast() {
        try {

            ContextHolder.context.applicationContext.unregisterReceiver(InAppBroadcastReceiver())

        } catch (_: Exception) {
            //   e.printStackTrace()
        } catch (_: Throwable) {
            //  ex.printStackTrace()

        }
    }

    fun sendBroadCast(intent: Intent, context: Context) {
        try {
            val broadCastIntent = Intent(intent.action)
            broadCastIntent.putExtras(intent.extras!!)
            ContextHolder.context.applicationContext.sendBroadcast(broadCastIntent)
        } catch (_: Exception) {
            //e.printStackTrace()
        } catch (_: Throwable) {
            // ex.printStackTrace()

        }
    }


    fun isDeeplink(targetUrl: String): Boolean {
        val inAppDeeplink = Prefs.inAppDeeplink
        return inAppDeeplink.isNotEmpty()
                && targetUrl.isNotEmpty()
                && targetUrl.startsWith(
            inAppDeeplink,
            ignoreCase = true
        )
    }

    /*  fun getSdkDefaultObj():SdkParameters
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

        } catch (_: Exception) {

            restartApplication = true
        }

        return restartApplication
    }

    fun getClassName(className: String?): String? {
        try {
            if (Prefs.className.isEmpty()) {
                return className
            }
            return Prefs.className
        } catch (_: Exception) {
        } catch (_: Throwable) {
        }
        return className
    }

    fun generateRandomInt(): Int {
        var randomInteger: Int = Random().nextInt()
        while (Constants.listOfNotificationIds.contains(randomInteger)) {
            randomInteger = Random().nextInt()
        }
        return randomInteger

    }

    fun getNotificationPreference(): Int {
        try {
            return if (Prefs.notificationDisplayPriorityConfiguration == NotificationDisplayPriorityConfiguration.SHOW_WITH_DEFAULT_PRIORITY.ordinal) NotificationManager.IMPORTANCE_DEFAULT else NotificationManager.IMPORTANCE_HIGH
        } catch (_: java.lang.Exception) {
        } catch (_: Throwable) {
        }
        return NotificationManager.IMPORTANCE_DEFAULT
    }

    fun getChannelId(message: Message): String {
        val baseChannelId = if (message.sound.isNullOrEmpty()) Constants.NOTIFICATION_CHANNEL_ID + "_" + getNotificationPreference() else message.sound + "_" + getNotificationPreference()
        return if (message.muted == true) {
            "${baseChannelId}_muted"
        } else {
            baseChannelId
        }
    }

    fun getLanguage(): String {
        return Prefs.language.toString()
    }

    private fun hasFineOrCoarsePermission(context: Context): Boolean {
        return ContextCompat.checkSelfPermission(
            context,
            Manifest.permission.ACCESS_FINE_LOCATION
        ) == PackageManager.PERMISSION_GRANTED ||
                ContextCompat.checkSelfPermission(
                    context,
                    Manifest.permission.ACCESS_COARSE_LOCATION
                ) == PackageManager.PERMISSION_GRANTED
    }

    internal fun getLocationPermissionStatusString(context: Context): String {

        return if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.P) {
            if (hasFineOrCoarsePermission(context)) {
                "always"
            } else {
                "none"
            }
        } else {
            if (ContextCompat.checkSelfPermission(
                    context,
                    Manifest.permission.ACCESS_BACKGROUND_LOCATION
                ) == PackageManager.PERMISSION_GRANTED
            ) {
                "always"
            } else {
                if (hasFineOrCoarsePermission(context)) {
                    "appinuse"
                } else {
                    "none"
                }
            }
        }
    }


}
