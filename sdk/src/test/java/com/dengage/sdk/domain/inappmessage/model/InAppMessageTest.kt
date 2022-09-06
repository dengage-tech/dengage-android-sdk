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
            screenNameFilters = null
        )
        val displayTiming = DisplayTiming(
            delay = 10,
            showEveryXMinutes = 5
        )
        val inAppMessageData = InAppMessageData(
            messageDetails = "messageDetails",
            expireDate = "expireDate",
            priority = Priority.LOW.priority,
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming,
            publicId = null
        )

        val inAppMessage = InAppMessage(
            id = id,
            data = inAppMessageData
        )

        Assert.assertEquals(id, inAppMessage.id)
        Assert.assertEquals(inAppMessageData, inAppMessage.data)
        Assert.assertFalse(inAppMessage.data.isRealTime())
    }

}