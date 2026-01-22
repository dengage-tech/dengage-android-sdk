package com.dengage.sdk.domain.configuration.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class SdkParameters(
    @SerializedName("appId") val appId: String?,
    @SerializedName("accountId") val accountId: Int?,
    @SerializedName("accountName") val accountName: String?,
    @SerializedName("eventsEnabled") val eventsEnabled: Boolean,
    @SerializedName("inboxEnabled") val inboxEnabled: Boolean?,
    @SerializedName("inAppEnabled") val inAppEnabled: Boolean?,
    @SerializedName("geofenceEnabled") val geofenceEnabled: Boolean = true,
    @SerializedName("subscriptionEnabled") val subscriptionEnabled: Boolean?,
    @SerializedName("inAppFetchIntervalInMin") val inAppFetchIntervalInMin: Int?,
    @SerializedName("expiredMessagesFetchIntervalInMin") val expiredMessagesFetchIntervalInMin: Int?,
    @SerializedName("inAppMinSecBetweenMessages") val inAppMinSecBetweenMessages: Int?,
    @SerializedName("lastFetchTimeInMillis") var lastFetchTimeInMillis: Long = 0,
    @SerializedName("appTrackingEnabled") var appTrackingEnabled: Boolean = false,
    @SerializedName("appTrackingList") var appTrackingList: List<AppTracking>?,
    @SerializedName("realTimeInAppEnabled") val realTimeInAppEnabled: Boolean?,
    @SerializedName("realTimeInAppFetchIntervalInMinutes") val realTimeInAppFetchIntervalInMinutes: Int?,
    @SerializedName("realTimeInAppSessionTimeoutMinutes") val realTimeInAppSessionTimeoutMinutes: Int?,
    @SerializedName("surveyCheckEndpoint") val surveyCheckEndpoint: String?,
    @SerializedName("eventMappings") val eventMappings: List<EventMapping>?,
    @SerializedName("debugDeviceIds") val debugDeviceIds: List<String>?
) : Serializable
