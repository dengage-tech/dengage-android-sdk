package com.dengage.sdk.domain.inboxchannel.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inboxchannel.InboxChannelRepository
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelMessage
import com.dengage.sdk.util.createLazy

class GetInboxChannelMessages :
    CoroutineUseCase<MutableList<InboxChannelMessage>, GetInboxChannelMessages.Params>() {

    private val repository: InboxChannelRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): MutableList<InboxChannelMessage> =
        repository.getInboxChannelMessages(
            account = params!!.account,
            contactKey = params.contactKey,
            deviceId = params.deviceId,
            appId = params.appId,
            limit = params.limit
        )

    data class Params(
        val account: String,
        val contactKey: String?,
        val deviceId: String,
        val appId: String?,
        val limit: Int
    )
}
