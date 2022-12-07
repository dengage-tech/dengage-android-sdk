package com.dengage.sdk.domain.event.model

import org.junit.Assert
import org.junit.Test

class OpenEventTest {

    @Test
    fun `OpenEvent constructor test`() {
        val buttonId = "buttonId"
        val itemId = "itemId"
        val messageId = 1
        val messageDetails = "messageDetails"
        val integrationKey = "integrationKey"

        val openEvent = OpenEvent(
            buttonId = buttonId,
            itemId = itemId,
            messageId = messageId,
            messageDetails = messageDetails,
            integrationKey = integrationKey
        )

        Assert.assertEquals(buttonId, openEvent.buttonId)
        Assert.assertEquals(itemId, openEvent.itemId)
        Assert.assertEquals(messageId, openEvent.messageId)
        Assert.assertEquals(messageDetails, openEvent.messageDetails)
        Assert.assertEquals(integrationKey, openEvent.integrationKey)
    }

}