package com.dengage.sdk.domain.geofence.model.sync

import com.google.gson.annotations.SerializedName
import java.io.Serializable

/**
 * `GET /geofences/sync/{integrationKey}` cevabının kökü (contract §1).
 *
 * Tuning/remote config (topN, reevaluationDistanceMeters, adaptiveThreshold ...) bu cevapta DEĞİLDİR;
 * merkezi `SdkParameters.geofence` bloğundan okunur (contract §4).
 */
data class GeofenceSyncResponse(
    @SerializedName("etag") val etag: String? = null,
    @SerializedName("syncedAt") val syncedAt: String? = null,
    @SerializedName("horizonUntil") val horizonUntil: String? = null,
    @SerializedName("geofences") val geofences: List<SyncFence> = emptyList()
) : Serializable
