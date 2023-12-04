package com.dengage.sdk.manager.configuration

import android.annotation.SuppressLint
import android.content.pm.PackageManager
import android.os.DeadObjectException
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
import com.dengage.sdk.util.*
import java.util.concurrent.TimeUnit

class ConfigurationManager : BaseMvpManager<ConfigurationContract.View,
    ConfigurationContract.Presenter>(), ConfigurationContract.View {

    internal var configurationCallback: ConfigurationCallback? = null

    override fun providePresenter() = ConfigurationPresenter()

    internal fun setDomain()
    {
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
        DengageUtils.getMetaData(name = "den_in_app_api_url")?.let {
            Prefs.inAppApiBaseUrl = it
        }

        DengageUtils.getMetaData(name = "fetch_real_time_in_app_api_url")?.let {
            Prefs.getRealTimeMessagesBaseUrl = it
        }

    }
    internal fun init(
        firebaseApp: FirebaseApp?,
        firebaseIntegrationKey: String? = null
    ) {

        var subscription = Prefs.subscription
        if (subscription == null) {
            subscription = Subscription()
            Prefs.subscription = subscription
        }

        if (ConfigurationUtils.isGooglePlayServicesAvailable()) {
            DengageLogger.verbose("Google Play Services are available. Firebase services will be used")
            initWithGoogle(
                subscription = subscription,
                firebaseApp = firebaseApp,
                firebaseIntegrationKey= firebaseIntegrationKey
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
        try{
            val subscription = Prefs.subscription
            if (subscription?.integrationKey.isNullOrEmpty()) return
            if (!DengageUtils.isAppInForeground()) return
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
        catch (e:Exception){}
        catch (e:Throwable){}
    }

    override fun sdkParametersFetched(sdkParameters: SdkParameters) {
        configurationCallback?.fetchInAppMessages()
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
                subscription.token = it
                if (firebaseIntegrationKey != null) {
                    subscription.integrationKey=firebaseIntegrationKey
                }
                configurationCallback?.sendSubscription(subscription)
            }
        )

        ConfigurationUtils.getGmsAdvertisingId {
            if (subscription != null && (subscription.advertisingId != it || subscription.advertisingId.isNullOrEmpty())) {
                subscription.advertisingId = it
                if (firebaseIntegrationKey != null) {
                    subscription.integrationKey = firebaseIntegrationKey
                }
                configurationCallback?.sendSubscription(subscription)
            }
        }
    }

}