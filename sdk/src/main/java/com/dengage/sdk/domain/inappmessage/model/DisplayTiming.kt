package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class DisplayTiming(
    @SerializedName("triggerBy") var triggerBy: String,
    @SerializedName("delay") val delay: Int?,
    @SerializedName("showEveryXMinutes") val showEveryXMinutes: Int?
) : Serializable
