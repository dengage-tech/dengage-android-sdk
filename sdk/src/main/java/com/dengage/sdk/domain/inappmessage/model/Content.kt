package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class Content(
    @SerializedName("type") val type: String,
    @SerializedName("targetUrl") val targetUrl: String?,
    @SerializedName("props") val params: ContentParams,
    @SerializedName("contentId") val contentId: String? = null
) : Serializable
