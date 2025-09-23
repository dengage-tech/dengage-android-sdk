package com.dengage.sdk.domain.configuration.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class EventAttribute(
    @SerializedName("id") val id: Int?,
    @SerializedName("name") val name: String?,
    @SerializedName("dataType") val dataType: String?
) : Serializable
