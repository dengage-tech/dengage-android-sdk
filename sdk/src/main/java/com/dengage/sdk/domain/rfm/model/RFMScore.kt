package com.dengage.sdk.domain.rfm.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class RFMScore(
    @SerializedName("categoryId") var categoryId: String,
    @SerializedName("score") var score: Double
) : Serializable
