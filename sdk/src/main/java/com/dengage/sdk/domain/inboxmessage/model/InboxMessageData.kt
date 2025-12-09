package com.dengage.sdk.domain.inboxmessage.model

import com.dengage.sdk.domain.push.model.CarouselItem
import com.dengage.sdk.domain.push.model.CustomParam
import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class InboxMessageData(
    @SerializedName("title") val title: String?,
    @SerializedName("message") val message: String?,
    @SerializedName("mediaUrl") var mediaUrl: String?,
    @SerializedName("androidMediaUrl") val androidMediaUrl: String?,
    @SerializedName("targetUrl") var targetUrl: String?,
    @SerializedName("androidTargetUrl") val androidTargetUrl: String?,
    @SerializedName("receiveDate") val receiveDate: String?,
    @SerializedName("androidCarouselContent") val carouselItems: List<CarouselItem>?,
    @SerializedName("customParameters") val customParameters: List<CustomParam>?
) : Serializable
