package com.dengage.sdk.manager.subscription

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.data.cache.PrefsOld
import com.dengage.sdk.data.remote.api.DeviceConfigurationPreference
import com.dengage.sdk.domain.configuration.model.TokenType
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.manager.base.BaseMvpManager
import com.dengage.sdk.manager.session.SessionManager
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.GsonHolder

class SubscriptionManager :
    BaseMvpManager<SubscriptionContract.View, SubscriptionContract.Presenter>(),
    SubscriptionContract.View {

    override fun providePresenter() = SubscriptionPresenter()

    private var firebaseIntegrationKey: String? = null
    private var huaweiIntegrationKey: String? = null
    private var deviceId: String? = null
    private var contactKey: String? = null
    private var partnerDeviceId: String? = null
    private var deviceConfigPref: DeviceConfigurationPreference? = null

    fun buildSubscription(
        firebaseIntegrationKey: String?,
        huaweiIntegrationKey: String?,
        deviceId: String?,
        deviceConfigPref: DeviceConfigurationPreference?,
        contactKey: String?,
        partnerDeviceId: String?,
    ) {
        this.deviceConfigPref = deviceConfigPref
        this.firebaseIntegrationKey = firebaseIntegrationKey
        this.huaweiIntegrationKey = huaweiIntegrationKey
        this.deviceId = deviceId
        this.partnerDeviceId = partnerDeviceId
        this.contactKey = contactKey
        // Migration from old sdk
        PrefsOld.subscription?.let {
            Prefs.subscription = it
            PrefsOld.subscription = null
        }
        Prefs.subscription = Prefs.subscription ?: Subscription().apply {
            integrationKey = firebaseIntegrationKey ?: huaweiIntegrationKey ?: ""
        }
    }

    internal fun sendSubscription() {
        presenter.enqueueSubscription(Prefs.subscription ?: Subscription())
    }

    internal fun setToken(token: String?) {
        val sub = Prefs.subscription ?: return
        if (!sub.token.isNullOrEmpty()) return
        sub.tokenType = when (deviceConfigPref) {
            DeviceConfigurationPreference.Google -> TokenType.FIREBASE.type
            else -> TokenType.HUAWEI.type
        }
        sub.integrationKey = if (sub.tokenType == TokenType.FIREBASE.type) {
            firebaseIntegrationKey.toString()
        } else {
            huaweiIntegrationKey.toString()
        }
        sub.token = token
        saveAndEnqueue(sub)
    }

    internal fun setDeviceId(deviceId: String) {
        Prefs.subscription?.let { sub ->
            if (sub.deviceId.isNullOrEmpty() || sub.deviceId != deviceId) {
                sub.deviceId = deviceId
                saveAndEnqueue(sub)
                DengageLogger.debug("setDeviceId: $deviceId")
            }
        }
    }

    internal fun setCountry(country: String) {
        Prefs.subscription?.let { sub ->
            if (sub.country.isNullOrEmpty() || sub.country != country) {
                sub.country = country
                saveAndEnqueue(sub)
                DengageLogger.debug("setCountry: $country")
            }
        }
    }

    // Returns `true` if the contactKey changed (for use in inboxManager tasks).
    internal fun setContactKey(contactKey: String?): Boolean {
        val sub = Prefs.subscription ?: return false
        return if (sub.contactKey == null || sub.contactKey != contactKey) {
            clearContactSpecificCaches()
            sub.contactKey = contactKey
            saveAndEnqueue(sub)
            DengageLogger.debug("setContactKey: $contactKey")
            true
        } else {
            false
        }
    }

    internal fun setUserPermission(userPermission: Boolean) {
        Prefs.subscription?.let { sub ->
            if (sub.permission == null || sub.permission != userPermission) {
                sub.permission = userPermission
                saveAndEnqueue(sub)
                DengageLogger.debug("setUserPermission: $userPermission")
            }
        }
    }

    internal fun setFirebaseIntegrationKey(integrationKey: String) {
        Prefs.subscription?.let { sub ->
            if (sub.integrationKey.isEmpty() || sub.integrationKey != integrationKey) {
                sub.integrationKey = integrationKey
                saveAndEnqueue(sub)
            }
        }
    }

    internal fun setHuaweiIntegrationKey(integrationKey: String) {
        Prefs.subscription?.let { sub ->
            if (sub.integrationKey.isEmpty() || sub.integrationKey != integrationKey) {
                sub.integrationKey = integrationKey
                saveAndEnqueue(sub)
            }
        }
    }

    internal fun setPartnerDeviceId(adid: String?) {
        Prefs.subscription?.let { sub ->
            if ((sub.partnerDeviceId.isNullOrEmpty() || sub.partnerDeviceId != adid) && adid != null) {
                sub.partnerDeviceId = adid
                saveAndEnqueue(sub)
            }
        }
    }

    internal fun setLanguage(language: String) {
        try {
            Prefs.subscription?.let { sub ->
                if (sub.language != language) {
                    sub.language = language
                    Prefs.language = language
                    saveAndEnqueue(sub)
                    DengageLogger.debug("setLanguage: $language")
                }
            }
        } catch (_: Throwable) {
        }
    }

    fun saveSubscription(subscription: Subscription) {
        try {
            if (subscription.deviceId.isNullOrEmpty()) {
                if (deviceId.isNullOrEmpty()) {
                    subscription.deviceId = DengageUtils.getDeviceId()
                } else {
                    subscription.deviceId = deviceId
                }
            } else if (!subscription.deviceId.equals(deviceId) && !deviceId.isNullOrEmpty()) {
                subscription.deviceId = deviceId
            }
            contactKey?.let { subscription.contactKey = it }
            partnerDeviceId?.let { subscription.partnerDeviceId = it }
            subscription.apply {
                carrierId = DengageUtils.getCarrier(ContextHolder.context)
                appVersion = DengageUtils.getAppVersion(ContextHolder.context)
                sdkVersion = DengageUtils.getSdkVersion()
                language = DengageUtils.getLanguage()
                timezone = DengageUtils.getIANAFormatTimeZone()
            }
            Prefs.subscription = subscription
            DengageLogger.verbose("saveSubscription: ${GsonHolder.gson.toJson(subscription)}")
        } catch (_: Throwable) {
        }
    }

    private fun clearContactSpecificCaches() {
        Prefs.inAppMessageFetchTime = 0L
        Prefs.inAppMessageShowTime = 0L
        Prefs.inAppMessages = null
        Prefs.inboxMessageFetchTime = 0L
        Prefs.visitCountItems = mutableListOf()
        Prefs.lastSessionStartTime = 0L
        Prefs.lastSessionDuration = 0L
        Prefs.lastSessionVisitTime = 0L
        SessionManager.getSessionId(force = true)
    }

    private fun saveAndEnqueue(subscription: Subscription) {
        saveSubscription(subscription)
        presenter.enqueueSubscription(subscription)
    }

    override fun subscriptionSent() = Unit
}
