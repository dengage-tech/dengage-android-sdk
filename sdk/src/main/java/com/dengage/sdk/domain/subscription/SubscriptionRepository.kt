package com.dengage.sdk.domain.subscription

import com.dengage.sdk.data.remote.api.ApiType
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.subscription.model.Subscription
import retrofit2.Response

class SubscriptionRepository {

    private val service: SubscriptionService by service(ApiType.PUSH)

    suspend fun sendSubscription(
        subscription: Subscription
    ): Response<Unit> {
        return service.sendSubscription(
            subscription = subscription
        )
    }
}
