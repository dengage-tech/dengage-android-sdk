package com.dengage.sdk.manager.configuration

import android.annotation.SuppressLint
import android.content.pm.PackageManager
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.model.AppTracking
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.configuration.model.TokenType
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.manager.base.BaseMvpManager
import com.dengage.sdk.manager.configuration.util.ConfigurationUtils
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.google.firebase.FirebaseApp
import java.util.*
import java.util.concurrent.TimeUnit

class ConfigurationManager : BaseMvpManager<ConfigurationContract.View,
    ConfigurationContract.Presenter>(), ConfigurationContract.View {

    internal var configurationCallback: ConfigurationCallback? = null

    override fun providePresenter() = ConfigurationPresenter()

    internal fun init(
        firebaseApp: FirebaseApp?
    ) {
        DengageUtils.getMetaData(name = "den_push_api_url").apply {
            if (this == null) {
                DengageLogger.error("Push api url not found on application manifest metadata")
                throw RuntimeException("Push api url not found on application manifest metadata")
            } else {
                Prefs.pushApiBaseUrl = this
            }
        }
        DengageUtils.getMetaData(name = "den_event_api_url").apply {
            if (this == null) {
                DengageLogger.error("Event api url not found on application manifest metadata")
                throw RuntimeException("Event api url not found on application manifest metadata")
            } else {
                Prefs.eventApiBaseUrl = this
            }
        }

        var subscription = Prefs.subscription
        if (subscription == null) {
            subscription = Subscription()
            Prefs.subscription = subscription
        }

        if (ConfigurationUtils.isGooglePlayServicesAvailable() && ConfigurationUtils.isHuaweiMobileServicesAvailable()) {
            DengageLogger.verbose("Google Play Services and Huawei Mobile Service are available. Firebase services will be used")
            initWithGoogle(
                subscription = subscription,
                firebaseApp = firebaseApp
            )
        } else if (ConfigurationUtils.isHuaweiMobileServicesAvailable()) {
            DengageLogger.verbose("Huawei Mobile Services is available")
            initWithHuawei(
                subscription = subscription,
            )
        } else if (ConfigurationUtils.isGooglePlayServicesAvailable()) {
            DengageLogger.verbose("Google Play Services is available")
            initWithGoogle(
                subscription = subscription,
                firebaseApp = firebaseApp
            )
        }
    }

    @SuppressLint("QueryPermissionsNeeded")
    internal fun getAppTrackingTags(appTrackings: List<AppTracking>?): MutableList<TagItem> {
        val tagItems = mutableListOf<TagItem>()

        // tracking time will be 0 for first tracking
        if (Prefs.appTrackingTime != 0L) {
            // time diff between now and last tracking time
            val timeDiff: Long = Calendar.getInstance().timeInMillis - Prefs.appTrackingTime
            val lastTrackingTime = TimeUnit.MILLISECONDS.toDays(timeDiff)
            // return if tracking was already done in last 6 days
            if (lastTrackingTime < 6) return tagItems
        }
        val packageManager = ContextHolder.context?.packageManager
        // get a list of installed apps.
        val packages = packageManager?.getInstalledApplications(PackageManager.GET_META_DATA)

        for (app in appTrackings ?: listOf()) {
            var isInstalled = false
            if (packages != null) {
                for (packageInfo in packages) {
                    if (packageInfo.packageName == app.packageName) {
                        isInstalled = true
                        break
                    }
                }
            }
            tagItems.add(TagItem("app-${app.alias}", if (isInstalled) "true" else "false"))
        }
        Prefs.appTrackingTime = Calendar.getInstance().timeInMillis

        return tagItems
    }

    internal fun getSdkParameters() {
        val subscription = Prefs.subscription
        if (subscription?.integrationKey.isNullOrEmpty()) return

        // if 24 hours passed after getting sdk params, you should get again
        val sdkParameters = Prefs.sdkParameters
        if (sdkParameters != null &&
            System.currentTimeMillis() < sdkParameters.lastFetchTimeInMillis + (24 * 60 * 60 * 1000)
        ) {
            // fetch in app messages
            configurationCallback?.fetchInAppMessages()
            return
        }
        presenter.getSdkParameters(
            integrationKey = subscription!!.integrationKey
        )
    }

    override fun sdkParametersFetched(sdkParameters: SdkParameters) {
        configurationCallback?.fetchInAppMessages()
        if (sdkParameters.appTrackingEnabled) {
            configurationCallback?.startAppTracking(sdkParameters.appTrackingList)
        }
    }

    private fun initWithGoogle(subscription: Subscription, firebaseApp: FirebaseApp?) {
        ConfigurationUtils.getFirebaseToken(
            firebaseApp = firebaseApp,
            onTokenResult = {
                subscription.tokenType = TokenType.FIREBASE.type
                subscription.token = it
                configurationCallback?.sendSubscription(subscription)
            }
        )

        ConfigurationUtils.getGmsAdvertisingId {
            subscription.advertisingId = it
            configurationCallback?.sendSubscription(subscription)
        }
    }

    private fun initWithHuawei(subscription: Subscription) {
        ConfigurationUtils.getHuaweiToken(
            onTokenResult = {
                subscription.tokenType = TokenType.HUAWEI.type
                subscription.token = it
                configurationCallback?.sendSubscription(subscription)
            }
        )

        ConfigurationUtils.getHmsAdvertisingId {
            subscription.advertisingId = it
            configurationCallback?.sendSubscription(subscription)
        }
    }
}