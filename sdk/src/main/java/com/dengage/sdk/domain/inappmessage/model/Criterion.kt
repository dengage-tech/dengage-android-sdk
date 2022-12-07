package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class Criterion(
    @SerializedName("id") val id: Long,
    @SerializedName("parameter") val parameter: String,
    @SerializedName("dataType") val dataType: String,
    @SerializedName("comparison") val operator: String,
    @SerializedName("values") val values: List<String>?
) : Serializable
