package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class InlineTarget(

    @SerializedName("androidSelector") val androidSelector: Int?
) : Serializable