package com.dengage.sdk.manager.inappmessage.util

import androidx.test.platform.app.InstrumentationRegistry
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.event.model.ClientEvent
import com.dengage.sdk.domain.inappmessage.model.Criterion
import com.dengage.sdk.domain.inappmessage.model.EventFilter
import com.dengage.sdk.domain.inappmessage.model.TimeWindow
import com.dengage.sdk.util.ContextHolder
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

@Config(manifest = Config.NONE)
@RunWith(RobolectricTestRunner::class)
class EventHistoryUtilsTest {

    @Before
    fun setup() {
        ContextHolder.context = InstrumentationRegistry.getInstrumentation().context
        // Clear client events before each test
        Prefs.clientEvents = mutableMapOf()
    }

    @Test
    fun `operateEventHistoryFilter should return false when event is null`() {
        val criterion = createCriterion(
            event = null,
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
            event = "page_view",
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
        val recentEvent = createClientEvent("page_view", currentTime - (2 * 24 * 60 * 60 * 1000)) // 2 days ago
        val oldEvent = createClientEvent("page_view", currentTime - (10 * 24 * 60 * 60 * 1000)) // 10 days ago
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(recentEvent, oldEvent))

        val criterion = createCriterion(
            event = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "7", "DAY")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count only 1 event (recent one)
    }

    @Test
    fun `operateEventHistoryFilter should filter events by time window - hours`() {
        val currentTime = System.currentTimeMillis()
        val recentEvent = createClientEvent("page_view", currentTime - (30 * 60 * 1000)) // 30 minutes ago
        val oldEvent = createClientEvent("page_view", currentTime - (3 * 60 * 60 * 1000)) // 3 hours ago
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(recentEvent, oldEvent))

        val criterion = createCriterion(
            event = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "1", "HOUR")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count only 1 event (recent one)
    }

    @Test
    fun `operateEventHistoryFilter should filter events by time window - minutes`() {
        val currentTime = System.currentTimeMillis()
        val recentEvent = createClientEvent("page_view", currentTime - (5 * 60 * 1000)) // 5 minutes ago
        val oldEvent = createClientEvent("page_view", currentTime - (20 * 60 * 1000)) // 20 minutes ago
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(recentEvent, oldEvent))

        val criterion = createCriterion(
            event = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "10", "MINUTE")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count only 1 event (recent one)
    }

    @Test
    fun `operateEventHistoryFilter should apply filters with AND logic`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("page_type" to "product", "category" to "shoes"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("page_type" to "category", "category" to "shoes"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("page_type", "EQUALS", listOf("product")),
            EventFilter("category", "EQUALS", listOf("shoes"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
            EventFilter("page_type", "EQUALS", listOf("product")),
            EventFilter("category", "EQUALS", listOf("shoes"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
            event = "page_view",
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
            event = "page_view",
            aggregateType = "DISTINCT_COUNT",
            field = "product_id",
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
            event = "page_view",
            aggregateType = "DISTINCT_COUNT",
            field = null,
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
            event = "page_view",
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
            event = "page_view",
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
            event = "page_view",
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
            event = "page_view",
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
            event = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            timeWindow = TimeWindow("TIME", "1", "UNKNOWN_UNIT")
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
            event = "page_view",
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
            event = "page_view",
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
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("status" to "inactive"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("status", "EQUALS", listOf("active"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("status" to "inactive"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("status", "NOT_EQUALS", listOf("inactive"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("category" to "electronics"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("category" to "books"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("category", "IN", listOf("electronics", "clothing"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("category" to "books"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("category" to "electronics"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("category", "NOT_IN", listOf("electronics", "clothing"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("title" to "Product Description"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("title" to "Home Page"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("title", "LIKE", listOf("Product"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("title" to "Home Page"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("title" to "Product Description"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("title", "NOT_LIKE", listOf("Product"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("url" to "/products/123"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("url" to "/home"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("url", "STARTS_WITH", listOf("/products"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("url" to "/products/123"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("url", "NOT_STARTS_WITH", listOf("/products"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("filename" to "document.pdf"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("filename" to "image.jpg"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("filename", "ENDS_WITH", listOf(".pdf"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("filename" to "image.jpg"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("filename" to "document.pdf"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("filename", "NOT_ENDS_WITH", listOf(".pdf"))
        )

        val criterion = createCriterion(
            event = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters GREATER_THAN operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "100"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "50"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("price", "GREATER_THAN", listOf("75"))
        )

        val criterion = createCriterion(
            event = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters GREATER_EQUAL operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent1 = createClientEvent("page_view", currentTime, mapOf("price" to "100"))
        val matchingEvent2 = createClientEvent("page_view", currentTime, mapOf("price" to "75"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "50"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent1, matchingEvent2, nonMatchingEvent))

        val filters = listOf(
            EventFilter("price", "GREATER_EQUAL", listOf("75"))
        )

        val criterion = createCriterion(
            event = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("2"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters LESS_THAN operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "50"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "100"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent, nonMatchingEvent))

        val filters = listOf(
            EventFilter("price", "LESS_THAN", listOf("75"))
        )

        val criterion = createCriterion(
            event = "page_view",
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateEventHistoryFilter should filter events with filters LESS_EQUAL operator`() {
        val currentTime = System.currentTimeMillis()
        val matchingEvent1 = createClientEvent("page_view", currentTime, mapOf("price" to "50"))
        val matchingEvent2 = createClientEvent("page_view", currentTime, mapOf("price" to "75"))
        val nonMatchingEvent = createClientEvent("page_view", currentTime, mapOf("price" to "100"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(matchingEvent1, matchingEvent2, nonMatchingEvent))

        val filters = listOf(
            EventFilter("price", "LESS_EQUAL", listOf("75"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val events = listOf(createClientEvent("page_view", currentTime, mapOf("valid_field" to "value")))
        
        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val filters = listOf(
            EventFilter("non_existent_field", "EQUALS", listOf("value"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val events = listOf(createClientEvent("page_view", currentTime, mapOf("status" to "active")))
        
        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val filters = listOf(
            EventFilter("status", "INVALID_OPERATOR", listOf("active"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val events = listOf(createClientEvent("page_view", currentTime, mapOf("price" to "not_a_number")))
        
        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val filters = listOf(
            EventFilter("price", "GREATER_THAN", listOf("50"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
            EventFilter("price", "GREATER_THAN", listOf("not_a_number"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
            event = "page_view",
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
            event = "page_view",
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
        val event1 = createClientEvent("page_view", currentTime, mapOf("category" to "electronics", "price" to "100"))
        val event2 = createClientEvent("page_view", currentTime, mapOf("category" to "books", "price" to "20"))
        val event3 = createClientEvent("page_view", currentTime, mapOf("category" to "electronics", "price" to "50"))
        
        Prefs.clientEvents = mutableMapOf("page_view" to mutableListOf(event1, event2, event3))

        val filters = listOf(
            EventFilter("category", "EQUALS", listOf("electronics")),
            EventFilter("price", "GREATER_THAN", listOf("75"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
        val events = listOf(createClientEvent("page_view", currentTime, mapOf("category" to "Electronics")))
        
        Prefs.clientEvents = mutableMapOf("page_view" to events.toMutableList())

        val filters = listOf(
            EventFilter("category", "LIKE", listOf("electronics"))
        )

        val criterion = createCriterion(
            event = "page_view",
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
            event = "page_view",
            aggregateType = "DISTINCT_COUNT",
            field = "product_id",
            operator = "EQ",
            values = listOf("1")
        )

        val result = EventHistoryUtils.operateEventHistoryFilter(criterion)
        Assert.assertTrue(result) // Should count 1 distinct value (empty string)
    }

    private fun createCriterion(
        event: String?,
        aggregateType: String,
        operator: String,
        values: List<String>?,
        field: String? = null,
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
            aggregateField = field,
            eventType = event,
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
