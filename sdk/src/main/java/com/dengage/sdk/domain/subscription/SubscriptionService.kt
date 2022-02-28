package com.dengage.sdk.domain.subscription

import com.dengage.sdk.domain.subscription.model.Subscription
import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.Headers
import retrofit2.http.POST

interface SubscriptionService {

    @Headers("CONNECT_TIMEOUT:15000", "READ_TIMEOUT:15000", "WRITE_TIMEOUT:15000")
    @POST("/api/device/subscription")
    suspend fun sendSubscription(
        @Body subscription: Subscription
    ): Response<Unit>
}
