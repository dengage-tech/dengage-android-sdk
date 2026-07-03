package com.dengage.sdk.manager.inboxchannel

import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.callback.DengageError
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEvent
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelMessage
import com.dengage.sdk.domain.inboxchannel.usecase.GetInboxChannelMessages
import com.dengage.sdk.domain.inboxchannel.usecase.SendInboxChannelEvents
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.util.DengageLogger

class InboxChannelPresenter : BaseAbstractPresenter<InboxChannelContract.View>(),
    InboxChannelContract.Presenter {

    private val getInboxChannelMessages by lazy { GetInboxChannelMessages() }
    private val sendInboxChannelEvents by lazy { SendInboxChannelEvents() }

    override fun getInboxChannelMessages(
        limit: Int,
        dengageCallback: DengageCallback<MutableList<InboxChannelMessage>>
    ) {
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters
        if (!isInboxChannelEnabled(subscription, sdkParameters)) {
            dengageCallback.onResult(mutableListOf())
            return
        }

        getInboxChannelMessages(this) {
            onResponse = {
                view { fetchedInboxChannelMessages(it) }
                dengageCallback.onResult(it ?: mutableListOf())
            }
            onError = {
                dengageCallback.onError(DengageError(it.message))
            }
            params = GetInboxChannelMessages.Params(
                account = sdkParameters?.accountName!!,
                contactKey = subscription!!.getContactKeyForVisitorInfoParameter(),
                deviceId = subscription.getSafeDeviceId(),
                appId = sdkParameters.appId,
                limit = limit
            )
        }
    }

    override fun sendInboxChannelEvents(events: List<InboxChannelEvent>) {
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters
        if (!isInboxChannelEnabled(subscription, sdkParameters)) return
        if (events.isEmpty()) return

        sendInboxChannelEvents(this) {
            onResponse = {
                view { inboxChannelEventsSent() }
            }
            onError = {
                DengageLogger.error("sendInboxChannelEvents error: ${it.message}")
            }
            params = SendInboxChannelEvents.Params(
                account = sdkParameters?.accountName!!,
                contactKey = subscription!!.getContactKeyForVisitorInfoParameter(),
                deviceId = subscription.getSafeDeviceId(),
                appId = sdkParameters.appId,
                events = events
            )
        }
    }

    private fun isInboxChannelEnabled(
        subscription: Subscription?,
        sdkParameters: SdkParameters?
    ): Boolean {
        return subscription != null && sdkParameters?.accountName != null &&
                sdkParameters.inboxEnabled != null && sdkParameters.inboxEnabled
    }
}
