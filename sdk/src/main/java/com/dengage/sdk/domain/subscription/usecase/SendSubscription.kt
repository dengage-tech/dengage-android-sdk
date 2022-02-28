package com.dengage.sdk.domain.subscription.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.subscription.SubscriptionRepository
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SendSubscription : CoroutineUseCase<Response<Unit>, SendSubscription.Params>() {

    private val repository: SubscriptionRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.sendSubscription(
            subscription = params!!.subscription
        )

    data class Params(
        val subscription: Subscription
    )
}
