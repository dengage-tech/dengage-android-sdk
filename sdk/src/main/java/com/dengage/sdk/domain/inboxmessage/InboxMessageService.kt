package com.dengage.sdk.domain.inboxmessage

import com.dengage.sdk.domain.inboxmessage.model.InboxMessage
import okhttp3.ResponseBody
import retrofit2.Response
import retrofit2.http.GET
import retrofit2.http.Headers
import retrofit2.http.Path
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
        @Query("appId") appId: String?
    ): ResponseBody

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


    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @GET("/p/pi/setAsClickedAll")
    suspend fun setAllInboxMessagesAsClicked(
        @Query("appId") appId: String,
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @GET("/p/pi/setAsDeletedAll")
    suspend fun setAllInboxMessagesAsDeleted(
        @Query("appId") appId: String,
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
    ): Response<Unit>
}