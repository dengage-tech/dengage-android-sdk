package com.dengage.sdk.domain.configuration.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class EventAttribute(
    @SerializedName("name") val name: String?,
    @SerializedName("dataType") val dataType: String?,
    @SerializedName("tableColumnName") val tableColumnName: String?
) : Serializable
