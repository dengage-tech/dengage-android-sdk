package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class AbTestVariant(
    @SerializedName("contentId") val contentId: String?,
    @SerializedName("percentage") val percentage: Double = 0.0,
    @SerializedName("isControlGroup") val isControlGroup: Boolean = false,
    @SerializedName("type") val type: String?,
    @SerializedName("props") val params: ContentParams?
) : Serializable
