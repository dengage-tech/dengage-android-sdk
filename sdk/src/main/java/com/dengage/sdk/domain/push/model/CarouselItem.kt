package com.dengage.sdk.domain.push.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class CarouselItem(
    @SerializedName("id") val id: String = "",
    @SerializedName("title") val title: String = "",
    @SerializedName("desc") val description: String = "",
    @SerializedName("mediaUrl") val mediaUrl: String = "",
    @SerializedName("targetUrl") val targetUrl: String = "",
    @SerializedName("type") var type: String = "",
    @SerializedName("mediaFileLocation") var mediaFileLocation: String? = null,
    @SerializedName("mediaFileName") var mediaFileName: String? = null
) : Serializable
