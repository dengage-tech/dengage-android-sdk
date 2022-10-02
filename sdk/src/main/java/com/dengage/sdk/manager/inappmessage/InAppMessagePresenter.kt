package com.dengage.sdk.manager.inappmessage

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.usecase.*
import com.dengage.sdk.manager.base.BaseAbstractPresenter

class InAppMessagePresenter : BaseAbstractPresenter<InAppMessageContract.View>(),
    InAppMessageContract.Presenter {

    private val getInAppMessages by lazy { GetInAppMessages() }
    private val setInAppMessageAsClicked by lazy { SetInAppMessageAsClicked() }
    private val setInAppMessageAsDismissed by lazy { SetInAppMessageAsDismissed() }
    private val setInAppMessageAsDisplayed by lazy { SetInAppMessageAsDisplayed() }
    private val getInAppExpiredMessageIds by lazy { GetInAppExpiredMessageIds() }

    override fun getInAppMessages() {
        val sdkParameters = Prefs.sdkParameters
        val subscription = Prefs.subscription
        if (isInAppMessageEnabled(subscription, sdkParameters)) {

            // control next in app message fetch time
            if (System.currentTimeMillis() < Prefs.inAppMessageFetchTime) return

            val nextFetchTimePlus = (sdkParameters?.inAppFetchIntervalInMin ?: 0) * 60000
            Prefs.inAppMessageFetchTime = System.currentTimeMillis() + nextFetchTimePlus

            getInAppMessages(this) {
                onResponse = {
                    view { fetchedInAppMessages(it) }
                }
                onError = {
                    Prefs.inAppMessageFetchTime = System.currentTimeMillis()
                    view { showError(it) }
                }
                params = GetInAppMessages.Params(
                    account = sdkParameters?.accountName!!,
                    subscription = Prefs.subscription!!,
                    sdkParameters = sdkParameters

                )
            }
        }
    }

    override fun setInAppMessageAsDisplayed(messageDetails: String?) {
        val sdkParameters = Prefs.sdkParameters
        val subscription = Prefs.subscription

        if (isInAppMessageEnabled(subscription, sdkParameters)) {
            setInAppMessageAsDisplayed(this) {
                onResponse = {
                    view { inAppMessageSetAsDisplayed() }
                }
                params = SetInAppMessageAsDisplayed.Params(
                    account = sdkParameters?.accountName!!,
                    subscription = Prefs.subscription!!,
                    messageDetails = messageDetails
                )
            }
        }
    }

    override fun setInAppMessageAsClicked(
        inAppMessageId: String,
        messageDetails: String?,
        buttonId: String?
    ) {
        val sdkParameters = Prefs.sdkParameters
        val subscription = Prefs.subscription

        if (isInAppMessageEnabled(subscription, sdkParameters)) {
            // remove in app message from cache if clicked
            removeInAppMessageFromCache(inAppMessageId)

            setInAppMessageAsClicked(this) {
                onResponse = {
                    view { inAppMessageSetAsClicked() }
                }
                params = SetInAppMessageAsClicked.Params(
                    account = sdkParameters?.accountName!!,
                    subscription = Prefs.subscription!!,
                    messageDetails = messageDetails,
                    buttonId = buttonId
                )
            }
        }
    }

    override fun setInAppMessageAsDismissed(messageDetails: String?) {
        val sdkParameters = Prefs.sdkParameters
        val subscription = Prefs.subscription

        if (isInAppMessageEnabled(subscription, sdkParameters)) {
            setInAppMessageAsDismissed(this) {
                onResponse = {
                    view { inAppMessageSetAsDismissed() }
                }
                params = SetInAppMessageAsDismissed.Params(
                    account = sdkParameters?.accountName!!,
                    subscription = Prefs.subscription!!,
                    messageDetails = messageDetails
                )
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
        sdkParameters: SdkParameters?
    ): Boolean {
        return subscription != null && sdkParameters?.accountName != null &&
                sdkParameters.inAppEnabled != null && sdkParameters.inAppEnabled
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
        return Prefs.inAppMessages?.let { it.size > 0} ?: false
    }
}