package com.dengage.sdk.domain.inboxmessage

import com.dengage.sdk.data.remote.api.ApiType
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage
import com.dengage.sdk.domain.inboxmessage.model.InboxMessageParser
import com.dengage.sdk.util.extension.getCdKey
import com.dengage.sdk.util.extension.getType
import retrofit2.Response

class InboxMessageRepository {

    private val service: InboxMessageService by service(ApiType.PUSH)

    suspend fun getInboxMessages(
        account: String,
        subscription: Subscription,
        limit: Int,
        offset: Int,
        appId : String,
    ): MutableList<InboxMessage>? {
        val a = InboxMessageParser.parseInboxResponse( service.getInboxMessages(
            account = account,
            cdKey = subscription.getCdKey(),
            deviceId = subscription.getSafeDeviceId(),
            type = subscription.getType(),
            limit = limit,
            offset = offset,
            appId = appId

        ).string())

        return a

    }

    suspend fun setInboxMessageAsClicked(
        account: String,
        subscription: Subscription,
        messageId: String,
    ): Response<Unit> {
        return service.setInboxMessageAsClicked(
            account = account,
            cdKey = subscription.getCdKey(),
            deviceId = subscription.getSafeDeviceId(),
            type = subscription.getType(),
            messageId = messageId
        )
    }

    suspend fun setInboxMessageAsDeleted(
        account: String,
        subscription: Subscription,
        messageId: String,
    ): Response<Unit> {
        return service.setInboxMessageAsDeleted(
            account = account,
            cdKey = subscription.getCdKey(),
            deviceId = subscription.getSafeDeviceId(),
            type = subscription.getType(),
            messageId = messageId
        )
    }

    suspend fun setAllInboxMessagesAsClicked(
        appId : String,
        account: String,
        subscription: Subscription,
    ): Response<Unit> {
        return service.setAllInboxMessagesAsClicked(
            appId = appId,
            account = account,
            cdKey = subscription.getCdKey(),
            deviceId = subscription.getSafeDeviceId(),
            type = subscription.getType(),
        )
    }

    suspend fun setAllInboxMessagesAsDeleted(
        appId : String,
        account: String,
        subscription: Subscription,
    ): Response<Unit> {
        return service.setAllInboxMessagesAsDeleted(
            appId = appId,
            account = account,
            cdKey = subscription.getCdKey(),
            deviceId = subscription.getSafeDeviceId(),
            type = subscription.getType(),
        )
    }

}