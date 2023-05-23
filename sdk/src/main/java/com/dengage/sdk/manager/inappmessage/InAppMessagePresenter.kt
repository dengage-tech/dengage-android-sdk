package com.dengage.sdk.manager.inappmessage

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.configuration.usecase.GetVisitorInfo
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.usecase.*
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.manager.session.SessionManager

class InAppMessagePresenter : BaseAbstractPresenter<InAppMessageContract.View>(),
    InAppMessageContract.Presenter {

    private val getInAppMessages by lazy { GetInAppMessages() }
    private val getRealTimeInAppMessages by lazy { GetRealTimeInAppMessages() }
    private val setInAppMessageAsClicked by lazy { SetInAppMessageAsClicked() }
    private val setRealTimeInAppMessageAsClicked by lazy { SetRealTimeInAppMessageAsClicked() }
    private val setInAppMessageAsDismissed by lazy { SetInAppMessageAsDismissed() }
    private val setRealTimeInAppMessageAsDismissed by lazy { SetRealTimeInAppMessageAsDismissed() }
    private val setInAppMessageAsDisplayed by lazy { SetInAppMessageAsDisplayed() }
    private val getInAppExpiredMessageIds by lazy { GetInAppExpiredMessageIds() }
    private val setRealTimeInAppMessageAsDisplayed by lazy { SetRealTimeInAppMessageAsDisplayed() }
    private val getVisitorInfo by lazy { GetVisitorInfo() }

    override fun getInAppMessages() {
        try {
            val sdkParameters = Prefs.sdkParameters
            val subscription = Prefs.subscription
            if (isInAppMessageEnabled(subscription,
                    sdkParameters) && DengageUtils.isAppInForeground()
            ) {


                if (Prefs.isDevelopmentStatusDebug == false) {
                    if (System.currentTimeMillis() < Prefs.inAppMessageFetchTime) return

                    val nextFetchTimePlus = (sdkParameters?.inAppFetchIntervalInMin ?: 0) * 60000
                    Prefs.inAppMessageFetchTime = System.currentTimeMillis() + nextFetchTimePlus
                }
                getInAppMessages(this) {
                    onResponse = {
                        view {
                            fetchedInAppMessages(it, false)
                            fetchInAppExpiredMessageIds()
                        }
                    }
                    onError = {
                        //  Prefs.inAppMessageFetchTime = System.currentTimeMillis()
                        view { showError(it) }
                    }
                    params = GetInAppMessages.Params(
                        account = sdkParameters?.accountName!!,
                        subscription = Prefs.subscription!!,
                        sdkParameters = sdkParameters

                    )
                }
            }

            if (isRealTimeInAppMessageEnabled(subscription, sdkParameters) &&
                DengageUtils.isAppInForeground()
            ) {

                if (Prefs.isDevelopmentStatusDebug == false) {
                    if (System.currentTimeMillis() < Prefs.realTimeInAppMessageFetchTime) return

                    val nextFetchTimePlus = (sdkParameters?.realTimeInAppFetchIntervalInMinutes
                        ?: 0) * 60000
                    Prefs.realTimeInAppMessageFetchTime =
                        System.currentTimeMillis() + nextFetchTimePlus

                }

                getRealTimeInAppMessages(this) {
                    onResponse = {
                        view {
                            fetchedInAppMessages(it, true)
                        }
                    }
                    onError = {
                        Prefs.realTimeInAppMessageFetchTime = System.currentTimeMillis()
                        view { showError(it) }
                    }
                    params = GetRealTimeInAppMessages.Params(
                        accountId = sdkParameters?.accountName!!,
                        appId = sdkParameters.appId!!
                    )
                }

                // get visitor info for segments and tags defined to user
                if (subscription != null && sdkParameters != null) {
                    getVisitorInfo(this) {
                        onResponse = {
                            Prefs.visitorInfo = it
                        }
                        params = GetVisitorInfo.Params(
                            accountName = sdkParameters.accountName,
                            contactKey = subscription.contactKey,
                            deviceId = subscription.getSafeDeviceId(),
                        )
                    }
                }
            }
        } catch (e: Exception) {
        } catch (e: Throwable) {
        }

    }

    override fun setInAppMessageAsDisplayed(inAppMessage: InAppMessage) {
        val sdkParameters = Prefs.sdkParameters
        val subscription = Prefs.subscription

        if (inAppMessage.data.isRealTime()) {
            if (isRealTimeInAppMessageEnabled(subscription, sdkParameters)) {
                setRealTimeInAppMessageAsDisplayed(this) {
                    onResponse = {
                        view { inAppMessageSetAsDisplayed() }
                    }
                    params = SetRealTimeInAppMessageAsDisplayed.Params(
                        accountName = sdkParameters?.accountName!!,
                        subscription = Prefs.subscription!!,
                        appId = sdkParameters.appId,
                        sessionId = SessionManager.getSessionId(),
                        campaignId = inAppMessage.data.publicId!!,
                        messageDetails = inAppMessage.data.messageDetails,
                        contentId = inAppMessage.data.content.contentId
                    )
                }
            }
        } else {
            if (isInAppMessageEnabled(subscription, sdkParameters)) {
                setInAppMessageAsDisplayed(this) {
                    onResponse = {
                        view { inAppMessageSetAsDisplayed() }
                    }
                    params = SetInAppMessageAsDisplayed.Params(
                        account = sdkParameters?.accountName!!,
                        subscription = Prefs.subscription!!,
                        messageDetails = inAppMessage.data.messageDetails
                    )
                }
            }
        }
    }

    override fun setInAppMessageAsClicked(
        inAppMessage: InAppMessage,
        buttonId: String?,
    ) {
        val sdkParameters = Prefs.sdkParameters
        val subscription = Prefs.subscription

        if (inAppMessage.data.isRealTime()) {
            if (isRealTimeInAppMessageEnabled(subscription, sdkParameters)) {
                // remove in app message from cache if clicked
                removeInAppMessageFromCache(inAppMessage.id)

                setRealTimeInAppMessageAsClicked(this) {
                    onResponse = {
                        view { inAppMessageSetAsClicked() }
                    }
                    params = SetRealTimeInAppMessageAsClicked.Params(
                        accountName = sdkParameters?.accountName!!,
                        subscription = Prefs.subscription!!,
                        appId = sdkParameters.appId,
                        sessionId = SessionManager.getSessionId(),
                        campaignId = inAppMessage.data.publicId!!,
                        messageDetails = inAppMessage.data.messageDetails,
                        buttonId = buttonId,
                        contentId = inAppMessage.data.content.contentId
                    )
                }
            }
        } else {
            if (isInAppMessageEnabled(subscription, sdkParameters)) {
                // remove in app message from cache if clicked
                removeInAppMessageFromCache(inAppMessage.id)

                setInAppMessageAsClicked(this) {
                    onResponse = {
                        view { inAppMessageSetAsClicked() }
                    }
                    params = SetInAppMessageAsClicked.Params(
                        account = sdkParameters?.accountName!!,
                        subscription = Prefs.subscription!!,
                        messageDetails = inAppMessage.data.messageDetails,
                        buttonId = buttonId
                    )
                }
            }
        }
    }

    override fun setInAppMessageAsDismissed(inAppMessage: InAppMessage) {
        val sdkParameters = Prefs.sdkParameters
        val subscription = Prefs.subscription

        if (inAppMessage.data.isRealTime()) {
            if (isRealTimeInAppMessageEnabled(subscription, sdkParameters)) {
                setRealTimeInAppMessageAsDismissed(this) {
                    onResponse = {
                        view { inAppMessageSetAsDismissed() }
                    }
                    params = SetRealTimeInAppMessageAsDismissed.Params(
                        accountName = sdkParameters?.accountName!!,
                        subscription = Prefs.subscription!!,
                        appId = sdkParameters.appId,
                        sessionId = SessionManager.getSessionId(),
                        campaignId = inAppMessage.data.publicId!!,
                        messageDetails = inAppMessage.data.messageDetails,
                        contentId = inAppMessage.data.content.contentId
                    )
                }
            }
        } else {
            if (isInAppMessageEnabled(subscription, sdkParameters)) {
                setInAppMessageAsDismissed(this) {
                    onResponse = {
                        view { inAppMessageSetAsDismissed() }
                    }
                    params = SetInAppMessageAsDismissed.Params(
                        account = sdkParameters?.accountName!!,
                        subscription = Prefs.subscription!!,
                        messageDetails = inAppMessage.data.messageDetails
                    )
                }
            }
        }
    }

    override fun fetchInAppExpiredMessageIds() {
        val sdkParameters = Prefs.sdkParameters

        if (isInAppAvailableInCache()) {

            // control next in app message fetch time
            if (System.currentTimeMillis() < Prefs.inAppRemoveFetchTime) return

            val nextFetchTimePlus = (sdkParameters?.expiredMessagesFetchIntervalInMin ?: 0) * 60000
            Prefs.inAppRemoveFetchTime = System.currentTimeMillis() + nextFetchTimePlus

            getInAppExpiredMessageIds(this) {
                onResponse = {
                    it?.let {
                        it.forEach { removeInAppMessageFromCache(it.id) }
                    }
                }
                onError = {
                    Prefs.inAppRemoveFetchTime = System.currentTimeMillis()
                    view { showError(it) }
                }
                params = GetInAppExpiredMessageIds.Params(
                    account = sdkParameters?.accountName!!,
                    subscription = Prefs.subscription!!,
                    sdkParameters = sdkParameters

                )
            }
        }
    }

    private fun isInAppMessageEnabled(
        subscription: Subscription?,
        sdkParameters: SdkParameters?,
    ): Boolean {
        return subscription != null && sdkParameters?.accountName != null &&
                sdkParameters.inAppEnabled != null && sdkParameters.inAppEnabled
    }

    private fun isRealTimeInAppMessageEnabled(
        subscription: Subscription?,
        sdkParameters: SdkParameters?,
    ): Boolean {
        return subscription != null && sdkParameters?.accountName != null &&
                sdkParameters.appId != null &&
                sdkParameters.realTimeInAppEnabled != null &&
                sdkParameters.realTimeInAppEnabled
    }

    private fun removeInAppMessageFromCache(inAppMessageId: String) {
        val inAppMessages = Prefs.inAppMessages
        inAppMessages?.removeAll { inAppMessage -> inAppMessage.id == inAppMessageId }
        Prefs.inAppMessages = inAppMessages
    }

    fun updateInAppMessageOnCache(inAppMessage: InAppMessage) {
        val inAppMessages = Prefs.inAppMessages
        inAppMessages?.removeAll { message -> message.id == inAppMessage.id }
        inAppMessages?.add(inAppMessage)
        Prefs.inAppMessages = inAppMessages
    }

    private fun isInAppAvailableInCache(): Boolean {
        return Prefs.inAppMessages?.let { it.size > 0 } ?: false
    }
}
