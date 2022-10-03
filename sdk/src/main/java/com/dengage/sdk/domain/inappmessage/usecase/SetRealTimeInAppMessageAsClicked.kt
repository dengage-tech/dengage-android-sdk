package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inappmessage.RealTimeInAppMessageRepository
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SetRealTimeInAppMessageAsClicked : CoroutineUseCase<Response<Unit>, SetRealTimeInAppMessageAsClicked.Params>() {

    private val repository: RealTimeInAppMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.setRealTimeInAppMessageAsClicked(
            accountName = params!!.accountName,
            subscription = params.subscription,
            appId = params.appId,
            sessionId = params.sessionId,
            campaignId = params.campaignId,
            messageDetails = params.messageDetails,
            buttonId = params.buttonId
        )

    data class Params(
        val accountName: String?,
        val subscription: Subscription,
        val appId: String?,
        val sessionId: String,
        val campaignId: String,
        val messageDetails: String?,
        val buttonId: String?
    )
}
