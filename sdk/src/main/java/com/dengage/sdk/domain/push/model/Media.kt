package com.dengage.sdk.domain.push.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class Media(
    @SerializedName("url") val url: String? = "",
    @SerializedName("target") val target: String? = ""
) : Serializable
