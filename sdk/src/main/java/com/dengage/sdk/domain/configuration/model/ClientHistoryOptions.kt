package com.dengage.sdk.domain.configuration.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class ClientHistoryOptions(
    @SerializedName("maxEventCount") val maxEventCount: Int?,
    @SerializedName("timeWindowInMinutes") val timeWindowInMinutes: Int?
) : Serializable