package com.dengage.sdk.manager.subscription

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.data.cache.PrefsOld
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.manager.base.BaseMvpManager
import com.dengage.sdk.manager.session.SessionManager
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.GsonHolder
import java.util.*

class SubscriptionManager : BaseMvpManager<SubscriptionContract.View, SubscriptionContract.Presenter>(),
    SubscriptionContract.View {

    override fun providePresenter() = SubscriptionPresenter()

     fun buildSubscription(
        firebaseIntegrationKey: String?
    ) {
        // this is for migration from old sdk
        if (PrefsOld.subscription != null) {
            Prefs.subscription = PrefsOld.subscription
            PrefsOld.subscription = null
        }

        var subscription = Prefs.subscription
        if (subscription == null) {
            subscription = Subscription()
            subscription.integrationKey = firebaseIntegrationKey ?: ""
        }
        Prefs.subscription = subscription
    }

    internal fun sendSubscription() {
        var subscription = Prefs.subscription
        if (subscription == null) {
            subscription = Subscription()
        }
        presenter.sendSubscription(subscription = subscription)
    }

    internal fun setToken(token: String?) {
        val subscription = Prefs.subscription

        if (subscription != null) {
            subscription.token = token

            saveSubscription(subscription = subscription)

            // send to api
            presenter.sendSubscription(subscription = subscription)
        }
    }

    internal fun setDeviceId(deviceId: String) {
        val subscription = Prefs.subscription

        // control the last device id equals to new device id then send subscription
        if (subscription != null && subscription.deviceId != deviceId) {
            subscription.deviceId = deviceId
            DengageLogger.debug("deviceId: $deviceId")

            saveSubscription(subscription = subscription)

            // send to api
            presenter.sendSubscription(subscription = subscription)
        }
    }

    internal fun setCountry(country: String) {
        val subscription = Prefs.subscription

        // control the last country equals to new country then send subscription
        if (subscription != null && (subscription.country == null || subscription.country != country)) {
            subscription.country = country
            DengageLogger.debug("country: $country")

            saveSubscription(subscription = subscription)

            // send to api
            presenter.sendSubscription(subscription = subscription)
        }
    }

    internal fun setContactKey(contactKey: String?): Boolean {
        val subscription = Prefs.subscription

        // control the last contact key equals to new contact key then send subscription
        if (subscription != null && (subscription.contactKey == null || subscription.contactKey != contactKey)) {
            // clear cache if contact key has been changed
            Prefs.inAppMessageFetchTime = 0L
            Prefs.inAppMessageShowTime = 0L
            Prefs.inAppMessages = null
            Prefs.inboxMessageFetchTime = 0L
            Prefs.visitCountItems = mutableListOf()
            Prefs.lastSessionStartTime = 0L
            Prefs.lastSessionDuration = 0L
            Prefs.lastSessionVisitTime = 0L
            SessionManager.getSessionId(force = true)

            subscription.contactKey = contactKey
            DengageLogger.debug("contactKey: $contactKey")

            saveSubscription(subscription = subscription)

            // send to api
            presenter.sendSubscription(subscription = subscription)

            // if contact key changed, return true for inboxManager jobs
            return true
        }

        return false
    }

    internal fun setUserPermission(userPermission: Boolean) {
        val subscription = Prefs.subscription

        // control the last permission flag equals to new permission flag then send subscription
        if (subscription != null && (subscription.permission == null || subscription.permission != userPermission)) {
            subscription.permission = userPermission
            DengageLogger.debug("permission: $userPermission")

            saveSubscription(subscription = subscription)

            // send to api
            presenter.sendSubscription(subscription = subscription)
        }
    }

     fun saveSubscription(subscription: Subscription) {
        DengageLogger.verbose("saveSubscription method is called")

        if (subscription.deviceId.isNullOrEmpty()) {
            subscription.deviceId = DengageUtils.getDeviceId()
        }
        subscription.carrierId = DengageUtils.getCarrier(ContextHolder.context)
        subscription.appVersion = DengageUtils.getAppVersion(ContextHolder.context)
        subscription.sdkVersion = DengageUtils.getSdkVersion()
        subscription.language = Locale.getDefault().language

         subscription.timezone = DengageUtils.getIANAFormatTimeZone()
        DengageLogger.debug("subscriptionJson: ${GsonHolder.gson.toJson(subscription)}")

        // save to cache
        Prefs.subscription = subscription
    }

    internal fun setFirebaseIntegrationKey(integrationKey: String) {
        val subscription = Prefs.subscription

        if (subscription != null) {
            subscription.integrationKey = integrationKey

            saveSubscription(subscription = subscription)

            // send to api
            presenter.sendSubscription(subscription = subscription)
        }
    }

    internal fun setPartnerDeviceId(adid: String?) {
        val subscription = Prefs.subscription

        if (subscription != null) {
            if (adid != null) {
                subscription.partnerDeviceId = adid
            }

            saveSubscription(subscription = subscription)

            // send to api
            presenter.sendSubscription(subscription = subscription)
        }
    }

    override fun subscriptionSent() = Unit

}
