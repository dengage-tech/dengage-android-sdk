package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.inappmessage.InAppMessageRepository
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.util.createLazy

class GetInAppMessages : CoroutineUseCase<MutableList<InAppMessage>?, GetInAppMessages.Params>() {

    private val repository: InAppMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): MutableList<InAppMessage>? =
        repository.getInAppMessages(
            account = params!!.account,
            subscription = params.subscription
        )

    data class Params(
        val account: String,
        val subscription: Subscription
    )
}
