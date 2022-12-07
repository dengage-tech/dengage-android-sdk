package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class DisplayTiming(
    @SerializedName("delay") val delay: Int?,
    @SerializedName("showEveryXMinutes") val showEveryXMinutes: Int?,
    @SerializedName("maxShowCount") val maxShowCount: Int?
) : Serializable
