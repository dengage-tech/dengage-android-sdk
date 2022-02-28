package com.dengage.sdk.domain.push.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class CustomParam(
    @SerializedName("key") val key: String?,
    val value: String?,
) : Serializable
