package com.dengage.sdk.domain.inboxmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.inboxmessage.InboxMessageRepository
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage
import com.dengage.sdk.util.createLazy

class GetInboxMessages : CoroutineUseCase<MutableList<InboxMessage>?, GetInboxMessages.Params>() {

    private val repository: InboxMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): MutableList<InboxMessage>? =
        repository.getInboxMessages(
            account = params!!.account,
            subscription = params.subscription,
            limit = params.limit,
            offset = params.offset
        )

    data class Params(
        val account: String,
        val subscription: Subscription,
        val limit: Int,
        val offset: Int
    )
}
