package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inappmessage.InAppMessageRepository
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SendFirstLaunchEvent : CoroutineUseCase<Response<Unit>, SendFirstLaunchEvent.Params>() {

    private val repository: InAppMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.sendFirstLaunchEvent(
            accountName = params!!.accountName,
            subscription = params.subscription,
            appId = params.appId,
            sessionId = params.sessionId,
        )

    data class Params(
        val accountName: String?,
        val subscription: Subscription?,
        val appId: String?,
        val sessionId: String
    )
}
