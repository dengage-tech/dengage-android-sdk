package com.dengage.sdk.manager.inboxmessage

import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.callback.DengageError
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage
import com.dengage.sdk.domain.inboxmessage.usecase.GetInboxMessages
import com.dengage.sdk.domain.inboxmessage.usecase.SetInboxMessageAsClicked
import com.dengage.sdk.domain.inboxmessage.usecase.SetInboxMessageAsDeleted
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter

class InboxMessagePresenter : BaseAbstractPresenter<InboxMessageContract.View>(),
    InboxMessageContract.Presenter {

    private val getInboxMessages by lazy { GetInboxMessages() }
    private val setInboxMessageAsClicked by lazy { SetInboxMessageAsClicked() }
    private val setInboxMessageAsDeleted by lazy { SetInboxMessageAsDeleted() }

    private var inboxMessages: MutableList<InboxMessage>? = null

    override fun getInboxMessages(
        limit: Int,
        offset: Int,
        dengageCallback: DengageCallback<MutableList<InboxMessage>>
    ) {
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters
        if (isInboxMessageEnabled(subscription, sdkParameters)) {
            if (!inboxMessages.isNullOrEmpty() && offset == 0 &&
                System.currentTimeMillis() < Prefs.inboxMessageFetchTime + 600000
            ) {
                dengageCallback.onResult(inboxMessages ?: mutableListOf())
            } else {
                getInboxMessages(this) {
                    onResponse = {
                        Prefs.inboxMessageFetchTime = System.currentTimeMillis()
                        view { fetchedInboxMessages(it) }

                        if (offset == 0) {
                            inboxMessages = it
                        }
                        dengageCallback.onResult(it ?: mutableListOf())
                    }
                    onError = {
                        dengageCallback.onError(DengageError(it.message))
                    }
                    params = GetInboxMessages.Params(
                        account = sdkParameters?.accountName!!,
                        subscription = Prefs.subscription!!,
                        limit = limit,
                        offset = offset
                    )
                }
            }
        } else {
            dengageCallback.onResult(mutableListOf())
        }
    }

    override fun setInboxMessageAsClicked(messageId: String) {
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters
        if (isInboxMessageEnabled(subscription, sdkParameters)) {
            inboxMessages?.firstOrNull { inboxMessage ->
                inboxMessage.id == messageId
            }?.isClicked = true

            setInboxMessageAsClicked(this) {
                onResponse = {
                    view { inboxMessageDeleted() }
                }
                params = SetInboxMessageAsClicked.Params(
                    account = sdkParameters?.accountName!!,
                    subscription = Prefs.subscription!!,
                    messageId = messageId
                )
            }
        }
    }

    override fun setInboxMessageAsDeleted(messageId: String) {
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters
        if (isInboxMessageEnabled(subscription, sdkParameters)) {
            inboxMessages?.removeAll { inboxMessage ->
                inboxMessage.id == messageId
            }

            setInboxMessageAsDeleted(this) {
                onResponse = {
                    view { inboxMessageDeleted() }
                }
                params = SetInboxMessageAsDeleted.Params(
                    account = sdkParameters?.accountName!!,
                    subscription = Prefs.subscription!!,
                    messageId = messageId
                )
            }
        }
    }

    override fun clearInboxMessageCache() {
        inboxMessages?.clear()
    }

    private fun isInboxMessageEnabled(
        subscription: Subscription?,
        sdkParameters: SdkParameters?
    ): Boolean {
        return subscription != null && sdkParameters?.accountName != null &&
            sdkParameters.inboxEnabled != null && sdkParameters.inboxEnabled
    }

}