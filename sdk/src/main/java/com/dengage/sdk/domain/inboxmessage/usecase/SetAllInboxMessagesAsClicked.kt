package com.dengage.sdk.domain.inboxmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.inboxmessage.InboxMessageRepository
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SetAllInboxMessagesAsClicked : CoroutineUseCase<Response<Unit>, SetAllInboxMessagesAsClicked.Params>() {

    private val repository: InboxMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.setAllInboxMessagesAsClicked(
            appId = params!!.appId,
            account = params.account,
            subscription = params.subscription,
        )

    data class Params(
        val appId: String,
        val account: String,
        val subscription: Subscription,
    )
}