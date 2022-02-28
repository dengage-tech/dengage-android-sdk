package com.dengage.sdk.domain.inboxmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.inboxmessage.InboxMessageRepository
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SetInboxMessageAsClicked : CoroutineUseCase<Response<Unit>, SetInboxMessageAsClicked.Params>() {

    private val repository: InboxMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.setInboxMessageAsClicked(
            account = params!!.account,
            subscription = params.subscription,
            messageId = params.messageId
        )

    data class Params(
        val account: String,
        val subscription: Subscription,
        val messageId: String
    )
}