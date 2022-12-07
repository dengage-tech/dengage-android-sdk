package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class InAppMessageData(
    @SerializedName("messageDetails") val messageDetails: String?,
    @SerializedName("expireDate") val expireDate: String,
    @SerializedName("priority") val priority: Int,
    @SerializedName("content") val content: Content,
    @SerializedName("displayCondition") val displayCondition: DisplayCondition,
    @SerializedName("displayTiming") val displayTiming: DisplayTiming,
    @SerializedName("publicId") val publicId: String?,
    @SerializedName("nextDisplayTime") var nextDisplayTime: Long = 0,
    @SerializedName("showCount") var showCount: Long = 0
) : Serializable {

    fun isRealTime(): Boolean = !publicId.isNullOrEmpty()

    fun isDisplayTimeAvailable(): Boolean {
        return (displayTiming.showEveryXMinutes == null ||
            displayTiming.showEveryXMinutes == 0 ||
            nextDisplayTime <= System.currentTimeMillis()) &&
            (displayTiming.maxShowCount == null ||
                displayTiming.maxShowCount == 0 ||
                showCount < displayTiming.maxShowCount)
    }
}
