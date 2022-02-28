package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class ScreenNameFilter(
    @SerializedName("operator") val operator: String,
    val value: List<String>
) : Serializable
