package com.dengage.sdk.domain.configuration.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class AppTracking(
    @SerializedName("alias") var alias: String,
    @SerializedName("packageName") var packageName: String
) : Serializable
