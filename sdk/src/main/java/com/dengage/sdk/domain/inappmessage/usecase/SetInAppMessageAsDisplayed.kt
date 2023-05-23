package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.inappmessage.InAppMessageRepository
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SetInAppMessageAsDisplayed : CoroutineUseCase<Response<Unit>, SetInAppMessageAsDisplayed.Params>() {

    private val repository: InAppMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.setInAppMessageAsDisplayed(
            account = params!!.account,
            subscription = params.subscription,
            messageDetails = params.messageDetails,
            contentId = params.contentId
        )

    data class Params(
        val account: String,
        val subscription: Subscription,
        val messageDetails: String?,
        val contentId: String?
    )
}
