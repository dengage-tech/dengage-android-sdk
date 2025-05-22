package com.dengage.sdk.manager.inboxmessage

import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.callback.DengageError
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage
import com.dengage.sdk.domain.inboxmessage.usecase.GetInboxMessages
import com.dengage.sdk.domain.inboxmessage.usecase.SetAllInboxMessagesAsClicked
import com.dengage.sdk.domain.inboxmessage.usecase.SetAllInboxMessagesAsDeleted
import com.dengage.sdk.domain.inboxmessage.usecase.SetInboxMessageAsClicked
import com.dengage.sdk.domain.inboxmessage.usecase.SetInboxMessageAsDeleted
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageUtils
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.TimeZone

class InboxMessagePresenter : BaseAbstractPresenter<InboxMessageContract.View>(),
    InboxMessageContract.Presenter {

    private val getInboxMessages by lazy { GetInboxMessages() }
    private val setInboxMessageAsClicked by lazy { SetInboxMessageAsClicked() }
    private val setInboxMessageAsDeleted by lazy { SetInboxMessageAsDeleted() }
    private val setAllInboxMessagesAsClicked by lazy { SetAllInboxMessagesAsClicked() }
    private val setAllInboxMessagesAsDeleted by lazy { SetAllInboxMessagesAsDeleted() }

    private var inboxMessages: MutableList<InboxMessage>? = null

    override fun getInboxMessages(
        limit: Int,
        offset: Int,
        dengageCallback: DengageCallback<MutableList<InboxMessage>>
    ) {
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters
        if (isInboxMessageEnabled(
                subscription,
                sdkParameters
            ) && DengageUtils.isAppInForeground()
        ) {
            if (!inboxMessages.isNullOrEmpty() && offset == 0 &&
                System.currentTimeMillis() < Prefs.inboxMessageFetchTime + 600000
            ) {
                dengageCallback.onResult(inboxMessages ?: mutableListOf())
            } else {
                getInboxMessages(this) {
                    onResponse = {
                        Prefs.inboxMessageFetchTime = System.currentTimeMillis()
                        view { fetchedInboxMessages(it) }


                            inboxMessages = it
                            updateInboxMessages(it)

                        dengageCallback.onResult(inboxMessages ?: mutableListOf())
                    }
                    onError = {
                        dengageCallback.onError(DengageError(it.message))
                    }
                    params = GetInboxMessages.Params(
                        account = sdkParameters?.accountName!!,
                        subscription = Prefs.subscription!!,
                        limit = limit,
                        offset = offset,
                        appId = sdkParameters.appId!!
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
            val message = inboxMessages?.firstOrNull { message ->
                message.id == messageId
            }
            if (message != null) {
                message.isClicked = true
                updateInboxMessagesPrefs(message)
            }

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

            val message = inboxMessages?.firstOrNull { message ->
                message.id == messageId
            }
            if (message != null) {
                message.isDeleted = true
                updateInboxMessagesPrefs(message)
            }

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

    override fun setAllInboxMessagesAsClicked() {
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters
        if (isInboxMessageEnabled(subscription, sdkParameters)) {
            setAllInboxMessagesAsClicked(this) {
                onResponse = {
                    inboxMessages?.forEach { message ->
                        message.isClicked = true
                    }
                    Prefs.inboxMessages = inboxMessages
                    view { allInboxMessagesClicked() }
                }
                params = SetAllInboxMessagesAsClicked.Params(
                    appId = sdkParameters?.appId!!,
                    account = sdkParameters.accountName!!,
                    subscription = Prefs.subscription!!
                )
            }
        }
    }

    override fun setAllInboxMessagesAsDeleted() {
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters
        if (isInboxMessageEnabled(subscription, sdkParameters)) {
            setAllInboxMessagesAsDeleted(this) {
                onResponse = {
                    inboxMessages?.forEach { message ->
                        message.isDeleted = true
                    }
                    Prefs.inboxMessages = inboxMessages
                    inboxMessages?.clear()
                    view { allInboxMessagesDeleted() }
                }
                params = SetAllInboxMessagesAsDeleted.Params(
                    appId = sdkParameters?.appId!!,
                    account = sdkParameters.accountName!!,
                    subscription = Prefs.subscription!!
                )
            }
        }
    }

    override fun clearInboxMessageCache() {
        inboxMessages?.clear()
        Prefs.inboxMessages = null
    }

    private fun isInboxMessageEnabled(
        subscription: Subscription?,
        sdkParameters: SdkParameters?
    ): Boolean {
        return subscription != null && sdkParameters?.accountName != null &&
                sdkParameters.inboxEnabled != null && sdkParameters.inboxEnabled
    }


    private fun updateInboxMessages(remoteInboxMessages: MutableList<InboxMessage>?) {

        if (remoteInboxMessages.isNullOrEmpty()) return

        val prefsInboxMessages = Prefs.inboxMessages
        if (prefsInboxMessages.isNullOrEmpty()) return

        remoteInboxMessages.forEach { remoteInboxMessage ->
            val matchingPrefsMessage = prefsInboxMessages.find { it.id == remoteInboxMessage.id }
            matchingPrefsMessage?.let {
                remoteInboxMessage.isClicked = it.isClicked
                remoteInboxMessage.isDeleted = it.isDeleted
            }
        }

        inboxMessages = remoteInboxMessages.filter { !it.isDeleted }.toMutableList()
    }


    private fun updateInboxMessagesPrefs(inboxMessage: InboxMessage) {
        val prefsInboxMessages = Prefs.inboxMessages?.toMutableList() ?: mutableListOf()

        val oneWeekAgo = System.currentTimeMillis() - 7L * 24 * 60 * 60 * 1000
        val filteredPrefsInboxMessages = prefsInboxMessages.filterNot {
            it.data.receiveDate?.let { receiveDate ->
                try {
                    val messageDate = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault()).apply {
                        timeZone = TimeZone.getTimeZone("UTC")
                    }.parse(receiveDate)?.time
                    messageDate != null && messageDate < oneWeekAgo
                } catch (e: Exception) {
                    true // If parsing fails, remove
                }
            } ?: false
        }.toMutableList()

        val existingMessageIndex = filteredPrefsInboxMessages.indexOfFirst { it.id == inboxMessage.id }

        if (existingMessageIndex != -1) {
            filteredPrefsInboxMessages[existingMessageIndex] = inboxMessage
        } else {
            filteredPrefsInboxMessages.add(inboxMessage)
        }

        Prefs.inboxMessages = filteredPrefsInboxMessages
    }



}