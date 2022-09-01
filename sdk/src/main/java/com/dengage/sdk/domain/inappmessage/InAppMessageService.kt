package com.dengage.sdk.domain.inappmessage

import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import retrofit2.Response
import retrofit2.http.GET
import retrofit2.http.Headers
import retrofit2.http.Query

interface InAppMessageService {

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/api/inapp/getMessages")
    suspend fun getInAppMessages(
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
        @Query("appid") appId:String
    ): MutableList<InAppMessage>?

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/api/inapp/setAsDisplayed")
    suspend fun setInAppMessageAsDisplayed(
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
        @Query("message_details") messageDetails: String?
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/api/inapp/setAsClicked")
    suspend fun setInAppMessageAsClicked(
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
        @Query("message_details") messageDetails: String?,
        @Query("button_id") buttonId: String?
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/api/inapp/setAsDismissed")
    suspend fun setInAppMessageAsDismissed(
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
        @Query("message_details") messageDetails: String?
    ): Response<Unit>
}
