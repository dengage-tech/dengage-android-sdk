package com.dengage.sdk.manager.configuration

import android.annotation.SuppressLint
import android.content.pm.PackageManager
import android.os.DeadObjectException
import android.os.Handler
import android.os.Looper
import androidx.core.app.NotificationManagerCompat
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.data.remote.api.ApiUrlConfiguration
import com.dengage.sdk.data.remote.api.DeviceConfigurationPreference
import com.dengage.sdk.data.remote.api.NotificationDisplayPriorityConfiguration
import com.dengage.sdk.domain.configuration.model.AppTracking
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.configuration.model.TokenType
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.manager.base.BaseMvpManager
import com.dengage.sdk.manager.configuration.util.ConfigurationUtils
import com.dengage.sdk.push.IDengageHmsManager
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.google.firebase.FirebaseApp
import java.util.*
import java.util.concurrent.TimeUnit
import kotlin.random.Random

class ConfigurationManager : BaseMvpManager<ConfigurationContract.View,
        ConfigurationContract.Presenter>(), ConfigurationContract.View {

    internal var configurationCallback: ConfigurationCallback? = null

    override fun providePresenter() = ConfigurationPresenter()

    internal fun setDomain(apiUrlConfiguration: ApiUrlConfiguration? = null, initForGeofence: Boolean = false) {

        if (apiUrlConfiguration == null) {
            mapOf(
                "den_push_api_url" to { url: String -> Prefs.pushApiBaseUrl = url },
                "den_event_api_url" to { url: String -> Prefs.eventApiBaseUrl = url },
                "den_in_app_api_url" to { url: String -> Prefs.inAppApiBaseUrl = url },
                "den_geofence_api_url" to { url: String -> Prefs.geofenceApiBaseUrl = url },
                "fetch_real_time_in_app_api_url" to { url: String -> Prefs.getRealTimeMessagesBaseUrl = url }
            ).forEach { (name, setter) ->
                DengageUtils.getMetaData(name = name)?.let(setter) ?: run {
                    DengageLogger.error("$name not found on application manifest metadata")
                    if(initForGeofence) {
                        throw RuntimeException("$name not found on application manifest metadata")
                    }
                }
            }
        } else {
            with(apiUrlConfiguration) {
                Prefs.pushApiBaseUrl = denPushApiUrl
                Prefs.eventApiBaseUrl = denEventApiUrl
                Prefs.inAppApiBaseUrl = denInAppApiUrl
                Prefs.geofenceApiBaseUrl = denGeofenceApiUrl
                Prefs.getRealTimeMessagesBaseUrl = fetchRealTimeInAppApiUrl
            }
        }
    }

    internal fun init(
        firebaseApp: FirebaseApp?,
        dengageHmsManager: IDengageHmsManager? = null,
        firebaseIntegrationKey: String? = null,
        huaweiIntegrationKey: String? = null,
        deviceConfigurationPreference: DeviceConfigurationPreference? = DeviceConfigurationPreference.Google,

        ) {

        var subscription = Prefs.subscription
        if (subscription == null) {
            subscription = Subscription()
            Prefs.subscription = subscription
        }

        ConfigurationUtils.dengageHmsManager = dengageHmsManager

        if (ConfigurationUtils.isGooglePlayServicesAvailable() && ConfigurationUtils.isHuaweiMobileServicesAvailable()) {
            DengageLogger.verbose("Google Play Services and Huawei Mobile Service are available. Firebase services will be used")
            if (deviceConfigurationPreference == DeviceConfigurationPreference.Google) {
                initWithGoogle(
                    subscription = subscription,
                    firebaseApp = firebaseApp,
                    firebaseIntegrationKey = firebaseIntegrationKey)
            } else {
                initWithHuawei(
                    subscription = subscription,
                    huaweiIntegrationKey = huaweiIntegrationKey
                )
            }
        } else if (ConfigurationUtils.isHuaweiMobileServicesAvailable()) {
            DengageLogger.verbose("Huawei Mobile Services is available")
            initWithHuawei(
                subscription = subscription,
                huaweiIntegrationKey = huaweiIntegrationKey
            )
        } else if (ConfigurationUtils.isGooglePlayServicesAvailable()) {
            DengageLogger.verbose("Google Play Services is available")
            initWithGoogle(
                subscription = subscription,
                firebaseApp = firebaseApp,
                firebaseIntegrationKey = firebaseIntegrationKey
            )
        }
    }

    @SuppressLint("QueryPermissionsNeeded")
    internal fun getAppTrackingTags(appTrackings: List<AppTracking>?): MutableList<TagItem> {
        val tagItems = mutableListOf<TagItem>()
        try {
            // tracking time will be 0 for first tracking
            if (Prefs.appTrackingTime != 0L) {
                // time diff between now and last tracking time
                val timeDiff: Long = Calendar.getInstance().timeInMillis - Prefs.appTrackingTime
                val lastTrackingTime = TimeUnit.MILLISECONDS.toDays(timeDiff)
                // return if tracking was already done in last 6 days
                if (lastTrackingTime < 6) return tagItems
            }
            val packageManager = ContextHolder.context.packageManager
            // get a list of installed apps.
            val packages = packageManager.getInstalledApplications(PackageManager.GET_META_DATA)

            for (app in appTrackings ?: listOf()) {
                var isInstalled = false
                for (packageInfo in packages) {
                    if (packageInfo.packageName == app.packageName) {
                        isInstalled = true
                        break
                    }
                }
                tagItems.add(TagItem("app-${app.alias}", if (isInstalled) "true" else "false"))
            }
            Prefs.appTrackingTime = Calendar.getInstance().timeInMillis
        } catch (ex: Throwable) {
            ex.printStackTrace()

        } catch (e: DeadObjectException) {
            e.printStackTrace()

        }
        return tagItems
    }

    fun getSdkParameters() {
        try {
            val subscription = Prefs.subscription
            if (subscription?.integrationKey.isNullOrEmpty()) return
            if (!DengageUtils.isAppInForeground()) return
            val sdkParameters = Prefs.sdkParameters

            if (sdkParameters == null) {
                presenter.getSdkParameters(integrationKey = subscription!!.integrationKey)
                return
            }

            if (System.currentTimeMillis() < sdkParameters.lastFetchTimeInMillis + (1 * 60 * 1000)) {
                // fetch in app messages
                configurationCallback?.fetchInAppMessages()
                return
            }

            Handler(Looper.getMainLooper()).postDelayed({
                presenter.getSdkParameters(integrationKey = subscription!!.integrationKey)
            }, Random.nextLong(0, 60000))
        } catch (e: Exception) {
            e.printStackTrace()
        } catch (e: Throwable) {
            e.printStackTrace()
        }
    }

    override fun sdkParametersFetched(sdkParameters: SdkParameters) {
        configurationCallback?.fetchInAppMessages()
        Dengage.cleanupClientEvents()
        if (sdkParameters.appTrackingEnabled) {
            configurationCallback?.startAppTracking(sdkParameters.appTrackingList)
        }
    }

    private fun initWithGoogle(
        subscription: Subscription,
        firebaseApp: FirebaseApp?,
        firebaseIntegrationKey: String? = null,
    ) {
        ConfigurationUtils.getFirebaseToken(
            firebaseApp = firebaseApp,
            onTokenResult = {
                subscription.tokenType = TokenType.FIREBASE.type
                Prefs.token = it
                val notificationsEnabled = NotificationManagerCompat.from(ContextHolder.context).areNotificationsEnabled()
                subscription.token = if (notificationsEnabled) it else ""
                if (firebaseIntegrationKey != null) {
                    subscription.integrationKey = firebaseIntegrationKey
                }
                configurationCallback?.sendSubscription(subscription)
            }
        )

        ConfigurationUtils.getGmsAdvertisingId { adId ->
            if (firebaseIntegrationKey != null) {
                subscription.integrationKey = firebaseIntegrationKey
            }
            subscription.advertisingId = adId ?: ""
            subscription.trackingPermission = !adId.isNullOrEmpty()
            configurationCallback?.sendSubscription(subscription)
        }
    }

    private fun initWithHuawei(
        subscription: Subscription,
        huaweiIntegrationKey: String? = null,
    ) {
        ConfigurationUtils.getHuaweiToken(
            onTokenResult = {
                subscription.tokenType = TokenType.HUAWEI.type
                Prefs.token = it
                val notificationsEnabled = NotificationManagerCompat.from(ContextHolder.context).areNotificationsEnabled()
                subscription.token = if (notificationsEnabled) it else ""
                if (huaweiIntegrationKey != null) {
                    subscription.integrationKey = huaweiIntegrationKey
                }
                configurationCallback?.sendSubscription(subscription)
            }
        )

        ConfigurationUtils.getHmsAdvertisingId {
            if (subscription.advertisingId != it || subscription.advertisingId.isEmpty()) {
                subscription.advertisingId = it
                if (huaweiIntegrationKey != null) {
                    subscription.integrationKey = huaweiIntegrationKey
                }
                configurationCallback?.sendSubscription(subscription)
            }
        }
    }

    fun saveOpenWebUrlConfigurations(disableOpenWebUrl: Boolean?) {
        try {
            Prefs.disableOpenWebUrl = disableOpenWebUrl
        } catch (e: java.lang.Exception) {
        } catch (e: Throwable) {
        }
    }

    fun saveNotificationPriority(notificationDisplayConfiguration: NotificationDisplayPriorityConfiguration?) {
        try {
            Prefs.notificationDisplayPriorityConfiguration = notificationDisplayConfiguration?.ordinal
        } catch (e: java.lang.Exception) {
        } catch (e: Throwable) {
        }
    }
}