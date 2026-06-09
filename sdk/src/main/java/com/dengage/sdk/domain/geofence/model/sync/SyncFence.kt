package com.dengage.sdk.domain.geofence.model.sync

import com.google.gson.annotations.SerializedName
import java.io.Serializable

/**
 * Cihazın izlemesi gereken tek bir fence (contract §1 `SyncFence`).
 * `activeNow` / `nextStateChangeAt` server-side hesaplanır (K7); cihaz bunları gate olarak kullanır.
 */
data class SyncFence(
    @SerializedName("fenceId") val fenceId: Int,
    @SerializedName("clusterId") val clusterId: Int,
    @SerializedName("latitude") val latitude: Double,
    @SerializedName("longitude") val longitude: Double,
    @SerializedName("radiusM") val radiusM: Double,
    @SerializedName("title") val title: String? = null,
    @SerializedName("activeNow") val activeNow: Boolean = false,
    @SerializedName("nextStateChangeAt") val nextStateChangeAt: String? = null,
    @SerializedName("nextStateChangeTo") val nextStateChangeTo: String? = null,
    @SerializedName("campaigns") val campaigns: List<SyncCampaign> = emptyList()
) : Serializable
