package com.dengage.sdk.domain.event

import com.dengage.sdk.domain.event.model.OpenEvent
import com.dengage.sdk.domain.event.model.PushEventPaths
import com.dengage.sdk.domain.event.model.TransactionalOpenEvent
import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.Headers
import retrofit2.http.POST

interface OpenEventService {

    @Headers("CONNECT_TIMEOUT:15000", "READ_TIMEOUT:15000", "WRITE_TIMEOUT:15000")
    @POST(PushEventPaths.TRANSACTIONAL_OPEN)
    suspend fun sendTransactionalOpenEvent(
        @Body transactionalOpenEvent: TransactionalOpenEvent
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:15000", "READ_TIMEOUT:15000", "WRITE_TIMEOUT:15000")
    @POST(PushEventPaths.OPEN)
    suspend fun sendOpenEvent(
        @Body openEvent: OpenEvent
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:15000", "READ_TIMEOUT:15000", "WRITE_TIMEOUT:15000")
    @POST(PushEventPaths.TRANSACTIONAL_DISMISS)
    suspend fun sendTransactionalDismissEvent(
        @Body transactionalOpenEvent: TransactionalOpenEvent
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:15000", "READ_TIMEOUT:15000", "WRITE_TIMEOUT:15000")
    @POST(PushEventPaths.DISMISS)
    suspend fun sendDismissEvent(
        @Body openEvent: OpenEvent
    ): Response<Unit>

}
