package com.dengage.sdk.domain.inboxmessage.model

import org.junit.Assert
import org.junit.Test

class InboxMessageTest {

    @Test
    fun `InboxMessage constructor test`() {
        val id = "id"
        val isClicked = false
        val title = "title"
        val message = "message"
        val mediaUrl = "mediaUrl"
        val androidMediaUrl = "androidMediaUrl"
        val targetUrl = "targetUrl"
        val androidTargetUrl = "androidTargetUrl"
        val receiveDate = "receiveDate"
        val carouselItems = null

        val inboxMessage = InboxMessage(
            id = id,
            isClicked = isClicked,
            data = InboxMessageData(
                title = title,
                message = message,
                mediaUrl = mediaUrl,
                androidMediaUrl = androidMediaUrl,
                targetUrl = targetUrl,
                androidTargetUrl = androidTargetUrl,
                receiveDate = receiveDate,
                carouselItems = carouselItems
            )
        )

        Assert.assertEquals(id, inboxMessage.id)
        Assert.assertEquals(isClicked, inboxMessage.isClicked)
        Assert.assertEquals(title, inboxMessage.data.title)
        Assert.assertEquals(message, inboxMessage.data.message)
        Assert.assertEquals(mediaUrl, inboxMessage.data.mediaUrl)
        Assert.assertEquals(androidMediaUrl, inboxMessage.data.androidMediaUrl)
        Assert.assertEquals(targetUrl, inboxMessage.data.targetUrl)
        Assert.assertEquals(androidTargetUrl, inboxMessage.data.androidTargetUrl)
        Assert.assertEquals(receiveDate, inboxMessage.data.receiveDate)
        Assert.assertEquals(carouselItems, inboxMessage.data.carouselItems)
    }

}