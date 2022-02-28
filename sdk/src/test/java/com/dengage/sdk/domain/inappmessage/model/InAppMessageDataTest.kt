package com.dengage.sdk.domain.inappmessage.model

import org.junit.Assert
import org.junit.Test

class InAppMessageDataTest {

    @Test
    fun `InAppMessageData constructor test`() {
        val messageId = Math.random().toString()
        val messageDetails = "messageDetails"
        val expireDate = "expireDate"
        val priority = Priority.HIGH.priority
        val dengageSendId = Math.random().toInt()
        val dengageCampId = Math.random().toInt()

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
            messageId = messageId,
            messageDetails = messageDetails,
            expireDate = expireDate,
            priority = priority,
            dengageSendId = dengageSendId,
            dengageCampId = dengageCampId,
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming
        )

        Assert.assertEquals(messageId, inAppMessageData.messageId)
        Assert.assertEquals(messageDetails, inAppMessageData.messageDetails)
        Assert.assertEquals(expireDate, inAppMessageData.expireDate)
        Assert.assertEquals(priority, inAppMessageData.priority)
        Assert.assertEquals(dengageSendId, inAppMessageData.dengageSendId)
        Assert.assertEquals(dengageCampId, inAppMessageData.dengageCampId)
        Assert.assertEquals(content, inAppMessageData.content)
        Assert.assertEquals(displayCondition, inAppMessageData.displayCondition)
        Assert.assertEquals(displayTiming, inAppMessageData.displayTiming)
    }

}