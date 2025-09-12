package com.dengage.sdk.manager.inappmessage.util

import androidx.test.platform.app.InstrumentationRegistry
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.event.model.ClientEvent
import com.dengage.sdk.domain.inappmessage.model.Criterion
import com.dengage.sdk.domain.inappmessage.model.EventFilter
import com.dengage.sdk.domain.inappmessage.model.TimeWindow
import com.dengage.sdk.manager.session.SessionManager
import com.dengage.sdk.util.ContextHolder
import org.junit.Assert
import org.junit.After
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import io.mockk.*


@Config(manifest = Config.NONE)
@RunWith(RobolectricTestRunner::class)
class EventHistoryUtilsTest {

    @Before
    fun setup() {
        ContextHolder.context = InstrumentationRegistry.getInstrumentation().context
        Prefs.clientEvents = mutableMapOf()
        mockkObject(SessionManager)
    }

    @After
    fun tearDown() {
        unmockkAll()
    }

    @Test
    fun `operateEventHistoryFilter should return false when event is null`() {
        val criterion = createCriterion(
            eventType = null,
            aggregateType = "COUNT",
            operator = "GT",
            values = listOf("1")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should return false when no events exist for event type`() {
        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "GT",
            values = listOf("1")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events by time window - days`() {
        // Setup events: one recent, one old
        val currentTime = System.currentTimeMillis()
        val recentEvent =
            createClientEvent("page_view", currentTime - (2 * 24 * 60 * 60 * 1000)) // 2 days ago
        val oldEvent =
            createClientEvent("page_view", currentTime - (10 * 24 * 60 * 60 * 1000)) // 10 days ago

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(recentEvent, oldEvent))

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "DAY", "7")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count only 1 event (recent one)
    }

    @Test
    fun `operateEventHistoryFilter should filter events by time window - hours`() {
        val currentTime = System.currentTimeMillis()
        val recentEvent =
            createClientEvent("page_view", currentTime - (30 * 60 * 1000)) // 30 minutes ago
        val oldEvent =
            createClientEvent("page_view", currentTime - (3 * 60 * 60 * 1000)) // 3 hours ago

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(recentEvent, oldEvent))

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "HOUR", "1")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count only 1 event (recent one)
    }

    @Test
    fun `operateEventHistoryFilter should filter events by time window - minutes`() {
        val currentTime = System.currentTimeMillis()
        val recentEvent =
            createClientEvent("page_view", currentTime - (5 * 60 * 1000)) // 5 minutes ago
        val oldEvent =
            createClientEvent("page_view", currentTime - (20 * 60 * 1000)) // 20 minutes ago

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(recentEvent, oldEvent))

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "MINUTE", "10")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count only 1 event (recent one)
    }

    @Test
    fun `operateEventHistoryFilter should apply filters with AND logic`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent(
            "page_view",
            currentTime,
            mapOf("page_type" to "product", "category" to "shoes")
        )
        val nonMatchingEvent = createClientEvent(
            "page_view",
            currentTime,
            mapOf("page_type" to "category", "category" to "shoes")
        )

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("page_type", "EQUALS", "TEXT", listOf("product")),
            EventFilter("category", "EQUALS", "TEXT", listOf("shoes"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count only 1 matching event
    }

    @Test
    fun `operateEventHistoryFilter should apply filters with OR logic`() {
        val currentTime = System.currentTimeMillis()
        val event1 = createClientEvent("page_view", currentTime, mapOf("page_type" to "product"))
        val event2 = createClientEvent("page_view", currentTime, mapOf("category" to "shoes"))
        val event3 = createClientEvent("page_view", currentTime, mapOf("page_type" to "category"))

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(event1, event2, event3))

        val filters = listOf(
            EventFilter("page_type", "EQUALS", "TEXT", listOf("product")),
            EventFilter("category", "EQUALS", "TEXT", listOf("shoes"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("2"),
            filters = filters,
            filtersLogicalOp = "OR"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count 2 events (event1 and event2)
    }

    @Test
    fun `operateEventHistoryFilter should count events correctly`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(
            createClientEvent("page_view", currentTime),
            createClientEvent("page_view", currentTime),
            createClientEvent("page_view", currentTime)
        )

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("3")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should count distinct values correctly`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(
            createClientEvent("page_view", currentTime, mapOf("product_id" to "123")),
            createClientEvent("page_view", currentTime, mapOf("product_id" to "456")),
            createClientEvent("page_view", currentTime, mapOf("product_id" to "123")) // duplicate
        )

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "EQ",
            values = listOf("2")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count 2 distinct product_ids
    }

    @Test
    fun `operateEventHistoryFilter should return false for distinct count when field is null`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "DISTINCT_COUNT",
            aggregateField = null,
            operator = "EQ",
            values = listOf("1")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should test all comparison operators`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(
            createClientEvent("page_view", currentTime),
            createClientEvent("page_view", currentTime),
            createClientEvent("page_view", currentTime)
        )

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        // Test EQUALS
        var criterion = createCriterion("page_view", "COUNT", "EQUALS", listOf("3"))
        Assert.assertTrue(EventHistoryUtils.operateEventHistoryFilter(criterion))

        // Test NOT_EQUALS
        criterion = createCriterion("page_view", "COUNT", "NOT_EQUALS", listOf("2"))
        Assert.assertTrue(EventHistoryUtils.operateEventHistoryFilter(criterion))

        // Test GREATER_THAN
        criterion = createCriterion("page_view", "COUNT", "GREATER_THAN", listOf("2"))
        Assert.assertTrue(EventHistoryUtils.operateEventHistoryFilter(criterion))

        // Test GREATER_EQUAL
        criterion = createCriterion("page_view", "COUNT", "GREATER_EQUAL", listOf("3"))
        Assert.assertTrue(EventHistoryUtils.operateEventHistoryFilter(criterion))

        // Test LESS_THAN
        criterion = createCriterion("page_view", "COUNT", "LESS_THAN", listOf("4"))
        Assert.assertTrue(EventHistoryUtils.operateEventHistoryFilter(criterion))

        // Test LESS_EQUAL
        criterion = createCriterion("page_view", "COUNT", "LESS_EQUAL", listOf("3"))
        Assert.assertTrue(EventHistoryUtils.operateEventHistoryFilter(criterion))
    }

    @Test
    fun `operateEventHistoryFilter should handle invalid operators`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "INVALID_OP",
            values = listOf("1")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should handle invalid aggregate types`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "INVALID_AGG",
            operator = "EQ",
            values = listOf("1")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should handle null values`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = null
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should handle invalid time window values`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "abc", "DAY") // invalid unit
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should use Long.MAX_VALUE as window, so all events included
    }

    @Test
    fun `operateEventHistoryFilter should handle unknown time window units`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "UNKNOWN_UNIT", "1")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should use Long.MAX_VALUE as window
    }

    @Test
    fun `operateEventHistoryFilter should handle exceptions gracefully`() {
        // Test with malformed criterion that might cause exceptions
        val criterion = Criterion(
            id = 1,
            parameter = "event_history",
            dataType = "INT",
            operator = "EQ",
            values = listOf("not_a_number"),
            valueSource = "BROWSER",
            aggregateType = "COUNT",
            aggregateField = null,
            eventType = "page_view",
            timeWindow = null,
            filtersLogicalOp = null,
            filters = null
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertFalse(result) // Should handle gracefully and return false
    }

    @Test
    fun `operateEventHistoryFilter should handle empty values list`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = emptyList()
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should handle non-numeric values for numeric comparison`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "GT",
            values = listOf("not_a_number")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters EQUALS operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("status" to "active"))
        val nonMatchingEvent =
            createClientEvent("page_view", currentTime, mapOf("status" to "inactive"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("status", "EQUALS", "TEXT", listOf("active"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters NOT_EQUALS operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("status" to "active"))
        val nonMatchingEvent =
            createClientEvent("page_view", currentTime, mapOf("status" to "inactive"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("status", "NOT_EQUALS", "TEXT", listOf("inactive"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters IN operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent =
            createClientEvent("page_view", currentTime, mapOf("category" to "electronics"))
        val nonMatchingEvent =
            createClientEvent("page_view", currentTime, mapOf("category" to "books"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("category", "IN", "TEXT", listOf("electronics", "clothing"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }


    @Test
    fun `operateEventHistoryFilter should filter events with filters NOT_IN operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent =
            createClientEvent("page_view", currentTime, mapOf("category" to "books"))
        val nonMatchingEvent =
            createClientEvent("page_view", currentTime, mapOf("category" to "electronics"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("category", "NOT_IN", dataType = "TEXT", listOf("electronics", "clothing"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }


    @Test
    fun `operateEventHistoryFilter should filter events with filters LIKE operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent =
            createClientEvent("page_view", currentTime, mapOf("title" to "Product Description"))
        val nonMatchingEvent =
            createClientEvent("page_view", currentTime, mapOf("title" to "Home Page"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("title", "LIKE", "TEXT", listOf("Product"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }


    @Test
    fun `operateEventHistoryFilter should filter events with filters NOT_LIKE operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent =
            createClientEvent("page_view", currentTime, mapOf("title" to "Home Page"))
        val nonMatchingEvent =
            createClientEvent("page_view", currentTime, mapOf("title" to "Product Description"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("title", "NOT_LIKE", "TEXT", listOf("Product"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters STARTS_WITH operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent =
            createClientEvent("page_view", currentTime, mapOf("url" to "/products/123"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("url" to "/home"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("url", "STARTS_WITH", "TEXT", listOf("/products"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters NOT_STARTS_WITH operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("url" to "/home"))
        val nonMatchingEvent =
            createClientEvent("page_view", currentTime, mapOf("url" to "/products/123"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("url", "NOT_STARTS_WITH", "TEXT", listOf("/products"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters ENDS_WITH operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent =
            createClientEvent("page_view", currentTime, mapOf("filename" to "document.pdf"))
        val nonMatchingEvent =
            createClientEvent("page_view", currentTime, mapOf("filename" to "image.jpg"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("filename", "ENDS_WITH", "TEXT", listOf(".pdf"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters NOT_ENDS_WITH operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent =
            createClientEvent("page_view", currentTime, mapOf("filename" to "image.jpg"))
        val nonMatchingEvent =
            createClientEvent("page_view", currentTime, mapOf("filename" to "document.pdf"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("filename", "NOT_ENDS_WITH", "TEXT", listOf(".pdf"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters GREATER_THAN operator for INT dataType`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "100"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "50"))

        Prefs.clientEvents =
            mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("price", "GREATER_THAN", "INT", listOf("75"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters GREATER_EQUAL operator for INT dataType`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent1 = createClientEvent("page_view", currentTime, mapOf("price" to "100"))
        val matchingEvent2 = createClientEvent("page_view", currentTime, mapOf("price" to "75"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "50"))

        Prefs.clientEvents = mutableMapOf(
            "page_view" to mutableListOf(
                matchingEvent1,
                matchingEvent2,
                nonMatchingEvent
            )
        )

        val filters = listOf(
            EventFilter("price", "GREATER_EQUAL", "INT", listOf("75"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("2"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters LESS_THAN operator for INT dataType`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "50"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "100"))

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("price", "LESS_THAN", "INT", listOf("75"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters LESS_EQUAL operator for INT dataType`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent1 = createClientEvent("page_view", currentTime, mapOf("price" to "50"))
        val matchingEvent2 = createClientEvent("page_view", currentTime, mapOf("price" to "75"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "100"))

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent1, matchingEvent2, nonMatchingEvent))

        val filters = listOf(
            EventFilter("price", "LESS_EQUAL", "INT", listOf("75"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("2"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should handle filters with invalid field names`() {
        val currentTime = System.currentTimeMillis()
        val events =
            listOf(createClientEvent("page_view", currentTime, mapOf("valid_field" to "value")))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val filters = listOf(
            EventFilter("non_existent_field", "EQUALS", "TEXT", listOf("value"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("0"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count 0 events since no events match the filter
    }

    @Test
    fun `operateEventHistoryFilter should handle filters with invalid operator`() {
        val currentTime = System.currentTimeMillis()
        val events =
            listOf(createClientEvent("page_view", currentTime, mapOf("status" to "active")))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val filters = listOf(
            EventFilter("status", "INVALID_OPERATOR", "TEXT", listOf("active"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("0"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count 0 events since filter doesn't match due to invalid operator
    }

    @Test
    fun `operateEventHistoryFilter should handle numeric comparison with non-numeric field values`() {
        val currentTime = System.currentTimeMillis()
        val events =
            listOf(createClientEvent("page_view", currentTime, mapOf("price" to "not_a_number")))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val filters = listOf(
            EventFilter("price", "GREATER_THAN", "TEXT", listOf("50"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("0"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count 0 events since non-numeric values don't match numeric comparisons
    }

    @Test
    fun `operateEventHistoryFilter should handle numeric comparison with non-numeric filter values`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime, mapOf("price" to "100")))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val filters = listOf(
            EventFilter("price", "GREATER_THAN", "TEXT", listOf("not_a_number"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("0"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count 0 events since filter value is not numeric
    }

    @Test
    fun `operateEventHistoryFilter should handle time window with invalid unit format`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "invalid_number", "DAY")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should include all events when time parsing fails
    }

    @Test
    fun `operateEventHistoryFilter should handle empty filters list but non-null`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(createClientEvent("page_view", currentTime))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = emptyList()
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should include all events when no filters are provided
    }

    @Test
    fun `operateEventHistoryFilter should handle mixed logical operators with complex filter combinations`() {
        val currentTime = System.currentTimeMillis()
        val event1 = createClientEvent(
            "page_view",
            currentTime,
            mapOf("category" to "electronics", "price" to "100")
        )
        val event2 = createClientEvent(
            "page_view",
            currentTime,
            mapOf("category" to "books", "price" to "20")
        )
        val event3 = createClientEvent(
            "page_view",
            currentTime,
            mapOf("category" to "electronics", "price" to "50")
        )

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(event1, event2, event3))

        val filters = listOf(
            EventFilter("category", "EQUALS", "TEXT", listOf("electronics")),
            EventFilter("price", "GREATER_THAN", "TEXT", listOf("75"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count 1 event that matches both filters
    }

    @Test
    fun `operateEventHistoryFilter should test case insensitive string comparisons`() {
        val currentTime = System.currentTimeMillis()
        val events =
            listOf(createClientEvent("page_view", currentTime, mapOf("category" to "Electronics")))

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val filters = listOf(
            EventFilter("category", "LIKE", "TEXT", listOf("electronics"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should match case-insensitively
    }

    @Test
    fun `operateEventHistoryFilter should handle distinct count with empty field value`() {
        val currentTime = System.currentTimeMillis()
        val events = listOf(
            createClientEvent("page_view", currentTime, mapOf("product_id" to "")),
            createClientEvent("page_view", currentTime, mapOf("other_field" to "value"))
        )

        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "EQ",
            values = listOf("1")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count 1 distinct value (empty string)
    }

    @Test
    fun `operateEventHistoryFilter should filter events by SESSION time window`() {
        val currentSessionId = "current_session_123"
        every { SessionManager.getSessionId(any()) } returns currentSessionId

        val currentTime = System.currentTimeMillis()

        // Create events: some in current session, some in different sessions
        val currentSessionEvent1 = createClientEvent(
            "add_to_cart", currentTime, mapOf(
                "product_id" to "123",
                "session_id" to currentSessionId
            )
        )
        val currentSessionEvent2 = createClientEvent(
            "add_to_cart", currentTime, mapOf(
                "product_id" to "456",
                "session_id" to currentSessionId
            )
        )
        val otherSessionEvent = createClientEvent(
            "add_to_cart", currentTime, mapOf(
                "product_id" to "789",
                "session_id" to "other_session_456"
            )
        )

        Prefs.clientEvents = mutableMapOf(
            "add_to_cart" to mutableListOf(
                currentSessionEvent1,
                currentSessionEvent2,
                otherSessionEvent
            )
        )

        // Create criterion matching the scenario: COUNT of add_to_cart events in SESSION > 1
        val criterion = createCriterion(
            eventType = "add_to_cart",
            aggregateType = "COUNT",
            operator = "GREATER_THAN",
            values = listOf("1"),
            timeWindow = TimeWindow("SESSION", "", ""),
            filters = emptyList(),
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)

        // Should return true because there are 2 add_to_cart events in current session, which is > 1
        Assert.assertTrue(result)

        // Verify that getSessionId was called
        verify { SessionManager.getSessionId(any()) }
    }

    @Test
    fun `operateEventHistoryFilter should return false for SESSION time window when count not met`() {
        val currentSessionId = "current_session_123"
        every { SessionManager.getSessionId(any()) } returns currentSessionId

        val currentTime = System.currentTimeMillis()

        // Create only one event in current session
        val currentSessionEvent = createClientEvent(
            "add_to_cart", currentTime, mapOf(
                "product_id" to "123",
                "session_id" to currentSessionId
            )
        )
        val otherSessionEvent = createClientEvent(
            "add_to_cart", currentTime, mapOf(
                "product_id" to "789",
                "session_id" to "other_session_456"
            )
        )

        Prefs.clientEvents = mutableMapOf(
            "add_to_cart" to mutableListOf(
                currentSessionEvent,
                otherSessionEvent
            )
        )

        // Create criterion: COUNT of add_to_cart events in SESSION > 1
        val criterion = createCriterion(
            eventType = "add_to_cart",
            aggregateType = "COUNT",
            operator = "GREATER_THAN",
            values = listOf("1"),
            timeWindow = TimeWindow("SESSION", "", ""),
            filters = emptyList(),
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)

        // Should return false because there is only 1 add_to_cart event in current session, which is not > 1
        Assert.assertFalse(result)

        // Verify that getSessionId was called
        verify { SessionManager.getSessionId(any()) }
    }

    @Test
    fun `operateEventHistoryFilter should handle SESSION time window with no current session events`() {
        val currentSessionId = "current_session_123"
        every { SessionManager.getSessionId(any()) } returns currentSessionId

        val currentTime = System.currentTimeMillis()

        // Create events only in other sessions
        val otherSessionEvent1 = createClientEvent(
            "add_to_cart", currentTime, mapOf(
                "product_id" to "123",
                "session_id" to "other_session_456"
            )
        )
        val otherSessionEvent2 = createClientEvent(
            "add_to_cart", currentTime, mapOf(
                "product_id" to "789",
                "session_id" to "other_session_789"
            )
        )

        Prefs.clientEvents = mutableMapOf(
            "add_to_cart" to mutableListOf(
                otherSessionEvent1,
                otherSessionEvent2
            )
        )

        // Create criterion: COUNT of add_to_cart events in SESSION > 1
        val criterion = createCriterion(
            eventType = "add_to_cart",
            aggregateType = "COUNT",
            operator = "GREATER_THAN",
            values = listOf("1"),
            timeWindow = TimeWindow("SESSION", "", ""),
            filters = emptyList(),
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)

        // Should return false because there are 0 add_to_cart events in current session
        Assert.assertFalse(result)

        // Verify that getSessionId was called
        verify { SessionManager.getSessionId(any()) }
    }




    // Senaryolu testleri:

    @Test
    fun `operateEventHistoryFilter should handle DISTINCT_COUNT with TIME window and filters - scenario 8 first criterion`() {
        val currentTime = System.currentTimeMillis()
        val withinRange = currentTime - (3 * 24 * 60 * 60 * 1000) // 3 days ago (within 7 days)
        val outsideRange = currentTime - (8 * 24 * 60 * 60 * 1000) // 8 days ago (outside 7 days)

        // Create page_view events with different product_ids and page_types
        val productPageEvent1 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_123",
            "page_type" to "product"
        ))
        val productPageEvent2 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_456",
            "page_type" to "product"
        ))
        val productPageEvent3 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_789",
            "page_type" to "product"
        ))
        val productPageEvent4 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_999",
            "page_type" to "product"
        ))
        val productPageEvent5 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_111",
            "page_type" to "product"
        ))
        val productPageEvent6 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_222",
            "page_type" to "product"
        ))

        // Add some events that should be filtered out
        val categoryPageEvent = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_333",
            "page_type" to "category" // different page_type, should be filtered out
        ))
        val oldProductPageEvent = createClientEvent("page_view", outsideRange, mapOf(
            "product_id" to "product_444",
            "page_type" to "product" // outside time window, should be filtered out
        ))

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(
            productPageEvent1,
            productPageEvent2,
            productPageEvent3,
            productPageEvent4,
            productPageEvent5,
            productPageEvent6,
            categoryPageEvent,
            oldProductPageEvent
        ))

        // Create criterion: DISTINCT_COUNT of page_view events by product_id > 5 in last 7 days with page_type = "product"
        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_THAN",
            values = listOf("5"),
            timeWindow = TimeWindow("TIME", "DAY", "7"),
            filters = listOf(
                EventFilter(
                    parameter = "page_type",
                    comparison = "EQUALS",
                    dataType = "TEXT",
                    values = listOf("product")
                )
            ),
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)

        // Should return true because there are 6 distinct product_ids in product page views within 7 days, which is > 5
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should handle COUNT with TIME window - scenario 8 second criterion`() {
        val currentTime = System.currentTimeMillis()
        val withinRange = currentTime - (3 * 24 * 60 * 60 * 1000) // 3 days ago (within 7 days)
        val outsideRange = currentTime - (8 * 24 * 60 * 60 * 1000) // 8 days ago (outside 7 days)

        // Create remove_from_cart events
        val removeEvent1 = createClientEvent(
            "remove_from_cart", withinRange, mapOf(
                "product_id" to "product_123"
            )
        )
        val removeEvent2 = createClientEvent(
            "remove_from_cart", withinRange, mapOf(
                "product_id" to "product_456"
            )
        )
        val removeEvent3 = createClientEvent(
            "remove_from_cart", outsideRange, mapOf(
                "product_id" to "product_789" // outside time window, should be filtered out
            )
        )

        Prefs.clientEvents = mutableMapOf(
            "remove_from_cart" to mutableListOf(
                removeEvent1,
                removeEvent2,
                removeEvent3
            )
        )

        // Create criterion: COUNT of remove_from_cart events > 1 in last 7 days
        val criterion = createCriterion(
            eventType = "remove_from_cart",
            aggregateType = "COUNT",
            operator = "GREATER_THAN",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "DAY", "7"),
            filters = emptyList(),
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)

        // Should return true because there are 2 remove_from_cart events within 7 days, which is > 1
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should return false when DISTINCT_COUNT criteria not met`() {
        val currentTime = System.currentTimeMillis()
        val withinRange = currentTime - (3 * 24 * 60 * 60 * 1000) // 3 days ago

        // Create only 3 page_view events with distinct product_ids (less than 5)
        val productPageEvent1 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_123",
            "page_type" to "product"
        ))
        val productPageEvent2 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_456",
            "page_type" to "product"
        ))
        val productPageEvent3 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_789",
            "page_type" to "product"
        ))

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(
            productPageEvent1,
            productPageEvent2,
            productPageEvent3
        ))

        // Create criterion: DISTINCT_COUNT of page_view events by product_id > 5
        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_THAN",
            values = listOf("5"),
            timeWindow = TimeWindow("TIME", "DAY", "7"),
            filters = listOf(
                EventFilter(
                    parameter = "page_type",
                    comparison = "EQUALS",
                    dataType = "TEXT",
                    values = listOf("product")
                )
            ),
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)

        // Should return false because there are only 3 distinct product_ids, which is not > 5
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should return false when COUNT criteria not met`() {
        val currentTime = System.currentTimeMillis()
        val withinRange = currentTime - (3 * 24 * 60 * 60 * 1000) // 3 days ago

        // Create only 1 remove_from_cart event (not > 1)
        val removeEvent = createClientEvent(
            "remove_from_cart", withinRange, mapOf(
                "product_id" to "product_123"
            )
        )

        Prefs.clientEvents = mutableMapOf("remove_from_cart" to mutableListOf(removeEvent))

        // Create criterion: COUNT of remove_from_cart events > 1
        val criterion = createCriterion(
            eventType = "remove_from_cart",
            aggregateType = "COUNT",
            operator = "GREATER_THAN",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "DAY", "7"),
            filters = emptyList(),
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)

        // Should return false because there is only 1 remove_from_cart event, which is not > 1
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should handle DISTINCT_COUNT with duplicate product_ids`() {
        val currentTime = System.currentTimeMillis()
        val withinRange = currentTime - (3 * 24 * 60 * 60 * 1000) // 3 days ago

        // Create multiple page_view events but with duplicate product_ids
        val productPageEvent1 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_123",
            "page_type" to "product"
        ))
        val productPageEvent2 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_123", // same product_id
            "page_type" to "product"
        ))
        val productPageEvent3 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_456",
            "page_type" to "product"
        ))
        val productPageEvent4 = createClientEvent("page_view", withinRange, mapOf(
            "product_id" to "product_456", // same product_id
            "page_type" to "product"
        ))

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(
            productPageEvent1,
            productPageEvent2,
            productPageEvent3,
            productPageEvent4
        ))

        // Create criterion: DISTINCT_COUNT of page_view events by product_id > 3
        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_THAN",
            values = listOf("3"),
            timeWindow = TimeWindow("TIME", "DAY", "7"),
            filters = listOf(
                EventFilter(
                    parameter = "page_type",
                    comparison = "EQUALS",
                    dataType = "TEXT",
                    values = listOf("product")
                )
            ),
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)

        // Should return false because there are only 2 distinct product_ids (123, 456), which is not > 3
        Assert.assertFalse(result)
    }

    @Test
    fun `operateEventHistoryFilter should handle TIME window edge cases`() {
        val currentTime = System.currentTimeMillis()
        val exactlySevenDaysAgo = currentTime - (7 * 24 * 60 * 60 * 1000) // exactly 7 days ago
        val justOverSevenDaysAgo = currentTime - (7 * 24 * 60 * 60 * 1000 + 1000) // just over 7 days ago

        // Create events at the edge of time window
        val edgeEvent = createClientEvent("page_view", exactlySevenDaysAgo, mapOf(
            "product_id" to "product_123",
            "page_type" to "product"
        ))
        val outsideEvent = createClientEvent("page_view", justOverSevenDaysAgo, mapOf(
            "product_id" to "product_456",
            "page_type" to "product"
        ))

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(
            edgeEvent,
            outsideEvent
        ))

        // Create criterion: DISTINCT_COUNT of page_view events by product_id > 0
        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_THAN",
            values = listOf("0"),
            timeWindow = TimeWindow("TIME", "DAY", "7"),
            filters = listOf(
                EventFilter(
                    parameter = "page_type",
                    comparison = "EQUALS",
                    dataType = "TEXT",
                    values = listOf("product")
                )
            ),
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)

        // The result depends on how the time window is implemented (inclusive vs exclusive)
        // This test documents the expected behavior
        Assert.assertTrue("Should include events at the edge of time window", result)
    }

    @Test
    fun `operateEventHistoryFilter should handle BOOL dataType with EQUALS operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("is_premium" to "true"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("is_premium" to "false"))

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("is_premium", "EQUALS", "BOOL", listOf("true"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should handle mixed dataTypes in filters`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf(
            "page_type" to "product",
            "price" to "100",
            "is_discounted" to "true"
        ))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf(
            "page_type" to "category",
            "price" to "50",
            "is_discounted" to "false"
        ))

        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("page_type", "EQUALS", "TEXT", listOf("product")),
            EventFilter("price", "GREATER_THAN", "INT", listOf("75")),
            EventFilter("is_discounted", "EQUALS", "BOOL", listOf("true"))
        )

        val criterion = createCriterion(
            eventType = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }


    private fun createCriterion(
        eventType: String?,
        aggregateType: String,
        operator: String,
        values: List<String>?,
        aggregateField: String? = null,
        timeWindow: TimeWindow? = null,
        filters: List<EventFilter>? = null,
        filtersLogicalOp: String? = null
    ): Criterion {
        return Criterion(
            id = 1,
            parameter = "event_history",
            dataType = "INT",
            operator = operator,
            values = values,
            valueSource = "BROWSER",
            aggregateType = aggregateType,
            aggregateField = aggregateField,
            eventType = eventType,
            timeWindow = timeWindow,
            filtersLogicalOp = filtersLogicalOp,
            filters = filters
        )
    }

    private fun createClientEvent(
        eventType: String,
        timestamp: Long,
        eventDetails: Map<String, Any> = emptyMap()
    ): ClientEvent {
        return ClientEvent(
            tableName = "events",
            key = "device_id",
            eventDetails = eventDetails,
            timestamp = timestamp,
            eventType = eventType
        )
    }
}
