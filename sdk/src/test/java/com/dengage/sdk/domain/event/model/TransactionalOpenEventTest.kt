package com.dengage.sdk.domain.event.model

import org.junit.Assert
import org.junit.Test

class TransactionalOpenEventTest {

    @Test
    fun `TransactionalOpenEvent constructor test`() {
        val buttonId = "buttonId"
        val itemId = "itemId"
        val messageId = 1
        val messageDetails = "messageDetails"
        val transactionId = "transactionId"
        val userAgent = "userAgent"
        val integrationKey = "integrationKey"

        val transactionalOpenEvent = TransactionalOpenEvent(
            buttonId = buttonId,
            itemId = itemId,
            messageId = messageId,
            messageDetails = messageDetails,
            transactionId = transactionId,
            userAgent = userAgent,
            integrationKey = integrationKey
        )

        Assert.assertEquals(buttonId, transactionalOpenEvent.buttonId)
        Assert.assertEquals(itemId, transactionalOpenEvent.itemId)
        Assert.assertEquals(messageId, transactionalOpenEvent.messageId)
        Assert.assertEquals(messageDetails, transactionalOpenEvent.messageDetails)
        Assert.assertEquals(transactionId, transactionalOpenEvent.transactionId)
        Assert.assertEquals(userAgent, transactionalOpenEvent.userAgent)
        Assert.assertEquals(integrationKey, transactionalOpenEvent.integrationKey)
    }

}