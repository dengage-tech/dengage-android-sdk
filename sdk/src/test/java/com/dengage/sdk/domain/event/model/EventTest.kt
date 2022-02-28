package com.dengage.sdk.domain.event.model

import org.junit.Assert
import org.junit.Test

class EventTest {

    @Test
    fun `Event constructor test`() {
        val integrationKey = "integrationKey"
        val key = "key"
        val eventTableName = "eventTableName"
        val eventDetails = mutableMapOf<String, Any>()
        eventDetails["test"] = "value"

        val event = Event(
            integrationKey = integrationKey,
            key = key,
            eventTableName = eventTableName,
            eventDetails = eventDetails
        )

        Assert.assertEquals(integrationKey, event.integrationKey)
        Assert.assertEquals(key, event.key)
        Assert.assertEquals(eventTableName, event.eventTableName)
        Assert.assertEquals(eventDetails, event.eventDetails)
    }

}