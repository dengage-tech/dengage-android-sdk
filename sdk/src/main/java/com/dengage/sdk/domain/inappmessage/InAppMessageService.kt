package com.dengage.sdk.domain.inappmessage

import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.model.InAppRemovalId
import com.dengage.sdk.domain.inappmessage.model.InAppMessageData
import retrofit2.Response
import retrofit2.http.GET
import retrofit2.http.Headers
import retrofit2.http.Path
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
        @Query("message_details") messageDetails: String?,
        @Query("content_id") contentId: String?
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/api/inapp/setAsClicked")
    suspend fun setInAppMessageAsClicked(
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
        @Query("message_details") messageDetails: String?,
        @Query("button_id") buttonId: String?,
        @Query("content_id") contentId: String?
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/api/inapp/setAsDismissed")
    suspend fun setInAppMessageAsDismissed(
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("did") deviceId: String,
        @Query("type") type: String,
        @Query("message_details") messageDetails: String?,
        @Query("content_id") contentId: String?
    ): Response<Unit>


    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/api/inapp/getExpiredMessages")
    suspend fun getInAppExpiredMessageIds(
        @Query("acc") account: String,
        @Query("cdkey") cdKey: String,
        @Query("appid") appId:String
    ): MutableList<InAppRemovalId>?

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/api/realtime-inapp/{accountId}/{appId}/campaign.json")
    suspend fun getRealTimeInAppMessages(
        @Path("accountId") accountId: String,
        @Path("appId") appId: String?,
    ): MutableList<InAppMessageData>?

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/realtime-inapp/event")
    suspend fun setRealTimeInAppMessageAsDisplayed(
        @Query("accid") accountName: String?,
        @Query("eventtype") eventType: String = "imp",
        @Query("ckey") contactKey: String?,
        @Query("did") deviceId: String,
        @Query("appid") appId: String?,
        @Query("session_id") sessionId: String,
        @Query("campid") campaignId: String,
        @Query("campparams") messageDetails: String?,
        @Query("content_id") contentId: String?
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/realtime-inapp/event")
    suspend fun setRealTimeInAppMessageAsClicked(
        @Query("accid") accountName: String?,
        @Query("eventtype") eventType: String = "cl",
        @Query("ckey") contactKey: String?,
        @Query("did") deviceId: String,
        @Query("appid") appId: String?,
        @Query("session_id") sessionId: String,
        @Query("campid") campaignId: String,
        @Query("campparams") messageDetails: String?,
        @Query("button_id") buttonId: String?,
        @Query("content_id") contentId: String?
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/realtime-inapp/event")
    suspend fun setRealTimeInAppMessageAsDismissed(
        @Query("accid") accountName: String?,
        @Query("eventtype") eventType: String = "dms",
        @Query("ckey") contactKey: String?,
        @Query("did") deviceId: String,
        @Query("appid") appId: String?,
        @Query("session_id") sessionId: String,
        @Query("campid") campaignId: String,
        @Query("campparams") messageDetails: String?,
        @Query("content_id") contentId: String?
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/realtime-inapp/event")
    suspend fun sendFirstLaunchEvent(
        @Query("accid") accountName: String?,
        @Query("eventtype") eventType: String = "firstlaunch",
        @Query("ckey") contactKey: String?,
        @Query("did") deviceId: String?,
        @Query("appid") appId: String?,
        @Query("session_id") sessionId: String
    ): Response<Unit>

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("/realtime-inapp/event")
    suspend fun sendAppForegroundEvent(
        @Query("accid") accountName: String?,
        @Query("eventtype") eventType: String = "foreground",
        @Query("ckey") contactKey: String?,
        @Query("did") deviceId: String,
        @Query("appid") appId: String?,
        @Query("session_id") sessionId: String,
        @Query("eventval") duration: Long
    ): Response<Unit>
}
