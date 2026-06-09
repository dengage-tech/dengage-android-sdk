package com.dengage.sdk.domain.geofence.model.sync

import com.google.gson.annotations.SerializedName
import java.io.Serializable

/**
 * Fence ile birlikte cache'lenen offline push içeriği (K10/K11, contract §1).
 * Offline trigger anında local notification network olmadan gösterilir.
 */
data class OfflinePushContent(
    @SerializedName("title") val title: String? = null,
    @SerializedName("body") val body: String? = null,
    @SerializedName("imageUrl") val imageUrl: String? = null,
    @SerializedName("deepLink") val deepLink: String? = null,
    @SerializedName("customParams") val customParams: Map<String, String>? = null
) : Serializable
