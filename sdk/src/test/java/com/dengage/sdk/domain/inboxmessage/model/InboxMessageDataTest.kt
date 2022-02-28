package com.dengage.sdk.domain.inboxmessage.model

import org.junit.Assert
import org.junit.Test

class InboxMessageDataTest {

    @Test
    fun `InboxMessageData constructor test`() {
        val title = "title"
        val message = "message"
        val mediaUrl = "mediaUrl"
        val androidMediaUrl = "androidMediaUrl"
        val targetUrl = "targetUrl"
        val androidTargetUrl = "androidTargetUrl"
        val receiveDate = "receiveDate"
        val carouselItems = null

        val inboxMessageData = InboxMessageData(
            title = title,
            message = message,
            mediaUrl = mediaUrl,
            androidMediaUrl = androidMediaUrl,
            targetUrl = targetUrl,
            androidTargetUrl = androidTargetUrl,
            receiveDate = receiveDate,
            carouselItems = carouselItems
        )

        Assert.assertEquals(title, inboxMessageData.title)
        Assert.assertEquals(message, inboxMessageData.message)
        Assert.assertEquals(mediaUrl, inboxMessageData.mediaUrl)
        Assert.assertEquals(androidMediaUrl, inboxMessageData.androidMediaUrl)
        Assert.assertEquals(targetUrl, inboxMessageData.targetUrl)
        Assert.assertEquals(androidTargetUrl, inboxMessageData.androidTargetUrl)
        Assert.assertEquals(receiveDate, inboxMessageData.receiveDate)
        Assert.assertEquals(carouselItems, inboxMessageData.carouselItems)
    }

}