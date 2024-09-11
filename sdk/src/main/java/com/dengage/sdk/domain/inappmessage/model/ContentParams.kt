package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class ContentParams(
    @SerializedName("position") val position: String,
    @SerializedName("shouldAnimate") val shouldAnimate: Boolean,
    @SerializedName("html") val html: String?,
    @SerializedName("maxWidth") val maxWidth: Int?,
    @SerializedName("radius") val radius: Int?,
    @SerializedName("marginTop") val marginTop: Int?,
    @SerializedName("marginBottom") val marginBottom: Int?,
    @SerializedName("marginLeft") val marginLeft: Int?,
    @SerializedName("marginRight") val marginRight: Int?,
    @SerializedName("dismissOnTouchOutside") val dismissOnTouchOutside: Boolean?,
    @SerializedName("backgroundColor") val backgroundColor: String?,
    @SerializedName("storySet") val storySet: StorySet?,

) : Serializable