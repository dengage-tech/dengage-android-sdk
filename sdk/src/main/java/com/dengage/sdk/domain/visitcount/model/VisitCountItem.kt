package com.dengage.sdk.domain.visitcount.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class VisitCountItem(
    @SerializedName("dateTime") val dateTime: Long,
    @SerializedName("visitCount") var visitCount: Int
) : Serializable
