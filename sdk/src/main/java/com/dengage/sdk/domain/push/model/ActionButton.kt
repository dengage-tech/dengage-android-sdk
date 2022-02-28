package com.dengage.sdk.domain.push.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class ActionButton(
    @SerializedName("id") val id: String?,
    @SerializedName("text") val text: String?,
    @SerializedName("icon") val icon: String?,
    @SerializedName("targetUrl") val targetUrl: String?
) : Serializable
