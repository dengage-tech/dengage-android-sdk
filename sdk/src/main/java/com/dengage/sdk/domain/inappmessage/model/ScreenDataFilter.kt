package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class ScreenDataFilter(
    @SerializedName("dataName") val dataName: String,
    @SerializedName("type") val type: String,
    @SerializedName("operator") val operator: String,
    val value: List<String>
) : Serializable
