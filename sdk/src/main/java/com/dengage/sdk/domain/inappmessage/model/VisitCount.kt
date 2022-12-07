package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class VisitCount(
    @SerializedName("count") val count: Int,
    @SerializedName("timeAmount") val timeAmount: Int
) : Serializable
