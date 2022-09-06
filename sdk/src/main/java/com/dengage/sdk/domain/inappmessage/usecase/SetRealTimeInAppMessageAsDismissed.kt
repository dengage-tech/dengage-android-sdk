package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inappmessage.InAppMessageRepository
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SetRealTimeInAppMessageAsDismissed : CoroutineUseCase<Response<Unit>, SetRealTimeInAppMessageAsDismissed.Params>() {

    private val repository: InAppMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.setRealTimeInAppMessageAsDismissed(
            accountName = params!!.accountName,
            subscription = params.subscription,
            appId = params.appId,
            sessionId = params.sessionId,
            campaignId = params.campaignId,
            messageDetails = params.messageDetails
        )

    data class Params(
        val accountName: String?,
        val subscription: Subscription,
        val appId: String?,
        val sessionId: String,
        val campaignId: String,
        val messageDetails: String?
    )
}
