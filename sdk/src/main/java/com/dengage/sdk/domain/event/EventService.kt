package com.dengage.sdk.domain.event

import com.dengage.sdk.domain.event.model.Event
import com.dengage.sdk.domain.event.model.LoginEvent
import com.dengage.sdk.domain.event.model.LogoutEvent
import com.dengage.sdk.domain.event.model.RegisterEvent
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

    @Headers("CONNECT_TIMEOUT:15000", "READ_TIMEOUT:15000", "WRITE_TIMEOUT:15000")
    @POST("/api/login")
    suspend fun sendLoginEvent(
        @Body event: LoginEvent
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:15000", "READ_TIMEOUT:15000", "WRITE_TIMEOUT:15000")
    @POST("/api/logout")
    suspend fun sendLogoutEvent(
        @Body event: LogoutEvent
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:15000", "READ_TIMEOUT:15000", "WRITE_TIMEOUT:15000")
    @POST("/api/register")
    suspend fun sendRegisterEvent(
        @Body event: RegisterEvent
    ): Response<Unit>

}