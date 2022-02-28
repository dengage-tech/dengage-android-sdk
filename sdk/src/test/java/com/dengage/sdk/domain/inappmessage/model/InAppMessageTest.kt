package com.dengage.sdk.domain.inappmessage.model

import org.junit.Assert
import org.junit.Test

class InAppMessageTest {

    @Test
    fun `InAppMessage constructor test`() {
        val id = Math.random().toString()
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
            screenNameFilters = null,
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
            expireDate = "expireDate",
            priority = Priority.LOW.priority,
            dengageSendId = Math.random().toInt(),
            dengageCampId = Math.random().toInt(),
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        val inAppMessage = InAppMessage(
            id = id,
            data = inAppMessageData
        )

        Assert.assertEquals(id, inAppMessage.id)
        Assert.assertEquals(inAppMessageData, inAppMessage.data)
    }

}