package com.dengage.sdk.domain.geofence.model.sync

import com.google.gson.annotations.SerializedName
import java.io.Serializable

/**
 * Bir fence'e bağlı kampanya metadata'sı (contract §1 `SyncCampaign`).
 */
data class SyncCampaign(
    @SerializedName("campaignId") val campaignId: Int,
    @SerializedName("campaignPublicId") val campaignPublicId: String,
    @SerializedName("triggerType") val triggerType: GeofenceTriggerType = GeofenceTriggerType.ENTER,
    @SerializedName("dwellMinutes") val dwellMinutes: Int? = null,
    @SerializedName("activeNow") val activeNow: Boolean = false,
    @SerializedName("nextStateChangeAt") val nextStateChangeAt: String? = null,
    @SerializedName("offlinePushContent") val offlinePushContent: OfflinePushContent? = null
) : Serializable
