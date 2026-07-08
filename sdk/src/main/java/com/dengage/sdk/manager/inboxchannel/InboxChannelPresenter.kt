package com.dengage.sdk.manager.inboxchannel

import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.callback.DengageError
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEvent
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEventType
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelMessage
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelMessageCache
import com.dengage.sdk.domain.inboxchannel.usecase.GetInboxChannelMessages
import com.dengage.sdk.domain.inboxchannel.usecase.SendInboxChannelEvents
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger
import java.text.SimpleDateFormat
import java.util.Locale
import java.util.TimeZone

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
                val messages = applyCachedState(it)
                view { fetchedInboxChannelMessages(messages) }
                dengageCallback.onResult(messages)
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

        // Reflect the interaction locally before the request so the UI/cache stay
        // in sync even though server processing is asynchronous.
        updateCacheForEvents(events)

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

    /**
     * Merges locally cached read/deleted state onto freshly fetched messages,
     * refreshes the cache with the latest receiveDate and drops deleted ones.
     */
    private fun applyCachedState(
        remoteMessages: MutableList<InboxChannelMessage>?
    ): MutableList<InboxChannelMessage> {
        if (remoteMessages.isNullOrEmpty()) return mutableListOf()

        val caches = prunedCaches().associateBy { it.id }.toMutableMap()

        remoteMessages.forEach { message ->
            caches[message.id]?.let { cache ->
                message.isRead = message.isRead || cache.isRead
                message.isDeleted = cache.isDeleted
            }
            caches[message.id] = InboxChannelMessageCache(
                id = message.id,
                isRead = message.isRead,
                isDeleted = message.isDeleted,
                receiveDate = message.data.receiveDate
            )
        }

        Prefs.inboxChannelMessageCaches = caches.values.toMutableList()

        return remoteMessages.filter { !it.isDeleted }.toMutableList()
    }

    /**
     * Updates the persisted cache for the given events: OPEN/CLICK mark a message
     * as read, DELETE marks it as deleted. IMPRESSION carries no state change.
     */
    private fun updateCacheForEvents(events: List<InboxChannelEvent>) {
        val caches = prunedCaches().associateBy { it.id }.toMutableMap()

        events.forEach { event ->
            val markRead = event.eventType == InboxChannelEventType.OPEN ||
                    event.eventType == InboxChannelEventType.CLICK
            val markDeleted = event.eventType == InboxChannelEventType.DELETE
            if (!markRead && !markDeleted) return@forEach

            val existing = caches[event.messageId]
            if (existing != null) {
                if (markRead) existing.isRead = true
                if (markDeleted) existing.isDeleted = true
            } else {
                caches[event.messageId] = InboxChannelMessageCache(
                    id = event.messageId,
                    isRead = markRead,
                    isDeleted = markDeleted,
                    receiveDate = null
                )
            }
        }

        Prefs.inboxChannelMessageCaches = caches.values.toMutableList()
    }

    private fun prunedCaches(): MutableList<InboxChannelMessageCache> {
        val caches = Prefs.inboxChannelMessageCaches?.toMutableList() ?: mutableListOf()
        val oneWeekAgo = System.currentTimeMillis() - 7L * 24 * 60 * 60 * 1000
        return caches.filterNot { cache ->
            val receiveDate = cache.receiveDate ?: return@filterNot false
            try {
                val messageDate = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault()).apply {
                    timeZone = TimeZone.getTimeZone("UTC")
                }.parse(receiveDate)?.time
                messageDate != null && messageDate < oneWeekAgo
            } catch (e: Exception) {
                false
            }
        }.toMutableList()
    }

    private fun isInboxChannelEnabled(
        subscription: Subscription?,
        sdkParameters: SdkParameters?
    ): Boolean {
        return subscription != null && sdkParameters?.accountName != null &&
                sdkParameters.inboxEnabled != null && sdkParameters.inboxEnabled
    }
}
