package com.dengage.sdk.domain.inappmessage.model

import org.junit.Assert
import org.junit.Test

class InAppMessageDataTest {

    @Test
    fun `InAppMessageData constructor test`() {
        val messageDetails = "messageDetails"
        val expireDate = "expireDate"
        val priority = Priority.HIGH.priority
        val publicId = "publicId"

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
            messageDetails = messageDetails,
            expireDate = expireDate,
            priority = priority,
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming,
            publicId = publicId
        )

        Assert.assertEquals(messageDetails, inAppMessageData.messageDetails)
        Assert.assertEquals(expireDate, inAppMessageData.expireDate)
        Assert.assertEquals(priority, inAppMessageData.priority)
        Assert.assertEquals(content, inAppMessageData.content)
        Assert.assertEquals(displayCondition, inAppMessageData.displayCondition)
        Assert.assertEquals(displayTiming, inAppMessageData.displayTiming)
        Assert.assertEquals(publicId, inAppMessageData.publicId)
        Assert.assertTrue(inAppMessageData.isRealTime())
    }

}