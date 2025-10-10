package com.dengage.sdk.domain.inappmessage

import com.google.gson.annotations.SerializedName
import retrofit2.Response
import retrofit2.http.*

interface DebugLoggingService {

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @POST("/felogging/{screenName}")
    suspend fun sendDebugLog(
        @Path("screenName") screenName: String,
        @Body request: DebugLogRequest
    ): Response<Unit>
}

data class DebugLogRequest(
    @SerializedName("trace_id") val traceId: String,
    @SerializedName("app_guid") val appGuid: String?,
    @SerializedName("app_id") val appId: String?,
    @SerializedName("account") val account: String?,
    @SerializedName("device") val device: String,
    @SerializedName("session_id") val sessionId: String,
    @SerializedName("sdk_version") val sdkVersion: String,
    @SerializedName("current_campaign_list") val currentCampaignList: List<String>,
    @SerializedName("campaign_id") val campaignId: String?,
    @SerializedName("campaign_type") val campaignType: String?,
    @SerializedName("send_id") val sendId: String?,
    @SerializedName("message") val message: String,
    @SerializedName("context") val context: Map<String, String>,
    @SerializedName("contact_key") val contactKey: String?,
    @SerializedName("channel") val channel: String,
    @SerializedName("current_rules") val currentRules: Map<String, Any>
)
