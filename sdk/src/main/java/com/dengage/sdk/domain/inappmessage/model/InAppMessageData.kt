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
    @SerializedName("inlineTarget") val inlineTarget: InlineTarget?,
    @SerializedName("publicId") val publicId: String?,
    @SerializedName("nextDisplayTime") var nextDisplayTime: Long = 0,
    @SerializedName("showCount") var showCount: Long = 0,
    @SerializedName("dismissCount") var dismissCount: Long = 0
) : Serializable {

    fun isRealTime(): Boolean = !publicId.isNullOrEmpty()

    /**
     * INLINE_CAROUSEL content is rendered inside a host view that controls its own
     * lifecycle, so the showEveryXMinutes and maxShowCount frequency caps do not
     * apply to it.
     */
    private fun isInlineCarousel(): Boolean =
        "INLINE_CAROUSEL".equals(content.type, ignoreCase = true)

    fun isDisplayTimeAvailable(): Boolean {

        if (isInlineCarousel()) return true

        if(displayTiming.showEveryXMinutes!=null &&displayTiming.maxShowCount!=null) {
            if (displayTiming.showEveryXMinutes == -1 || displayTiming.maxShowCount == -1) {
                return true
            }
        }
        return (displayTiming.showEveryXMinutes == null ||
            displayTiming.showEveryXMinutes == 0 ||
            nextDisplayTime <= System.currentTimeMillis()) &&
            (displayTiming.maxShowCount == null ||
                displayTiming.maxShowCount == 0 ||
                showCount < displayTiming.maxShowCount)
    }

    fun isDisplayTimeAvailable(
        context: MutableMap<String, String>,
        baseIndex: Int = 0
    ): Boolean {
        val currentTime = System.currentTimeMillis()
        var criterionIndex = baseIndex

        if (isInlineCarousel()) {
            // Still write both entries so the caller's criterionIndex accounting
            // (which advances by 2 for time constraints) stays aligned.
            context["show_every_x_minutes_${criterionIndex}"] =
                "noLimit|INLINE_CAROUSEL|TIME_CONSTRAINT|true"
            criterionIndex++
            context["max_show_count_${criterionIndex}"] =
                "noLimit|INLINE_CAROUSEL|SHOW_COUNT_CONSTRAINT|true"
            return true
        }

        // Check show every X minutes constraint
        val isTimeConstraintMet =
            if (displayTiming.showEveryXMinutes != null && displayTiming.showEveryXMinutes != 0 && displayTiming.showEveryXMinutes != -1) {
                val timeConstraintMet = nextDisplayTime <= currentTime
                context.let {
                    it["show_every_x_minutes_${criterionIndex}"] =
                        "${displayTiming.showEveryXMinutes}|$nextDisplayTime<=$currentTime|TIME_CONSTRAINT|$timeConstraintMet"
                    criterionIndex++
                }
                timeConstraintMet
            } else {
                context.let {
                    it["show_every_x_minutes_${criterionIndex}"] =
                        "noLimit|noLimit|TIME_CONSTRAINT|true"
                    criterionIndex++
                }
                true
            }

        // Check max show count constraint
        val isShowCountConstraintMet =
            if (displayTiming.maxShowCount != null && displayTiming.maxShowCount != 0 && displayTiming.maxShowCount != -1) {
                val showCountConstraintMet = showCount < displayTiming.maxShowCount
                context.let {
                    it["max_show_count_${criterionIndex}"] =
                        "${displayTiming.maxShowCount}|$showCount<${displayTiming.maxShowCount}|SHOW_COUNT_CONSTRAINT|$showCountConstraintMet"
                }
                showCountConstraintMet
            } else {
                context.let {
                    it["max_show_count_${criterionIndex}"] =
                        "noLimit|noLimit|SHOW_COUNT_CONSTRAINT|true"
                }
                true
            }

        return isTimeConstraintMet && isShowCountConstraintMet
    }
}
