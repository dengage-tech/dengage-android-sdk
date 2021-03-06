package com.dengage.sdk.domain.inboxmessage

import com.dengage.sdk.domain.inboxmessage.model.InboxMessage
import retrofit2.Response
import retrofit2.http.GET
import retrofit2.http.Headers
import retrofit2.http.Query

interface InboxMessageService {

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @GET("/api/pi/getMessages")
    suspend fun getInboxMessages(
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
        @Query("limit") limit: Int,
        @Query("offset") offset: Int,
    ): MutableList<InboxMessage>?

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @GET("/api/pi/setAsClicked")
    suspend fun setInboxMessageAsClicked(
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
        @Query("msgId") messageId: String,
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @GET("/api/pi/setAsDeleted")
    suspend fun setInboxMessageAsDeleted(
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
        @Query("msgId") messageId: String,
    ): Response<Unit>
}