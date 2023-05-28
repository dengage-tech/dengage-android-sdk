package com.dengage.sdk.domain.push.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class CarouselItem(
    @SerializedName("id") var id: String = "",
    @SerializedName("title") var title: String = "",
    @SerializedName("desc") var description: String = "",
    @SerializedName("mediaUrl") var mediaUrl: String = "",
    @SerializedName("targetUrl") var targetUrl: String = "",
    @SerializedName("type") var type: String = "",
    @SerializedName("mediaFileLocation") var mediaFileLocation: String? = null,
    @SerializedName("mediaFileName") var mediaFileName: String? = null
) : Serializable
