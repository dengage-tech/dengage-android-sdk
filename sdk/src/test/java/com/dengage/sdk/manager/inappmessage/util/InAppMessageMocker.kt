package com.dengage.sdk.manager.inappmessage.util

import com.dengage.sdk.domain.inappmessage.model.*

object InAppMessageMocker {

    fun createInAppMessage(
        id: String, priority: Priority, expireDate: String,
        screenName: String? = null, operator: Operator? = null
    ): InAppMessage {
        val contentParams = ContentParams(
            position = ContentPosition.BOTTOM.position,
            shouldAnimate = true,
            html = null,
            maxWidth = null,
            radius = null,
            marginTop = null,
            marginBottom = null,
            marginLeft = null,
            marginRight = null,
            dismissOnTouchOutside = false
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )

        val displayCondition = DisplayCondition(
            screenNameFilters = if (screenName != null && operator != null) listOf(
                ScreenNameFilter(
                    value = listOf(screenName),
                    operator = operator.operator
                )
            )
            else null,
            screenDataFilters = null
        )
        val displayTiming = DisplayTiming(
            triggerBy = TriggerBy.NAVIGATION.triggerBy,
            delay = 10,
            showEveryXMinutes = 5
        )
        val inAppMessageData = InAppMessageData(
            messageId = Math.random().toString(),
            messageDetails = "messageDetails",
            expireDate = expireDate,
            priority = priority.priority,
            dengageSendId = Math.random().toInt(),
            dengageCampId = Math.random().toInt(),
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        return InAppMessage(
            id = id,
            data = inAppMessageData
        )
    }

}