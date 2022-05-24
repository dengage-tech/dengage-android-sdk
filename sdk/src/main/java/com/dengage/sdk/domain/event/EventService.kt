package com.dengage.sdk.domain.event

import com.dengage.sdk.domain.event.model.Event
import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.Headers
import retrofit2.http.POST

interface EventService {

    @Headers("CONNECT_TIMEOUT:15000", "READ_TIMEOUT:15000", "WRITE_TIMEOUT:15000")
    @POST("/api/event")
    suspend fun sendEvent(
        @Body event: Event
    ): Response<Unit>
}