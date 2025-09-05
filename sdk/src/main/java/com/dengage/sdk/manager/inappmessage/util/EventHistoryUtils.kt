package com.dengage.sdk.manager.inappmessage.util

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.event.model.ClientEvent
import com.dengage.sdk.domain.inappmessage.model.Criterion
import com.dengage.sdk.domain.inappmessage.model.EventFilter
import com.dengage.sdk.domain.inappmessage.model.TimeWindow
import com.dengage.sdk.util.DengageLogger

object EventHistoryUtils {

    fun operateEventHistoryFilter(criterion: Criterion): Boolean {
        try {
            val eventType = criterion.eventType ?: return false
            val clientEvents = Prefs.clientEvents
            val eventTypeEvents = clientEvents[eventType] ?: return false

            val windowMillis = parseTimeWindow(criterion.timeWindow)
            val cutoffTime = System.currentTimeMillis() - windowMillis

            // Filter events by time window
            val eventsInWindow = eventTypeEvents.filter { it.timestamp >= cutoffTime }

            // Apply filters if present
            val filteredEvents = when {
                !criterion.filters.isNullOrEmpty() -> {
                    applyEventFilters(eventsInWindow, criterion.filters, criterion.filtersLogicalOp)
                }

                else -> {
                    eventsInWindow
                }
            }

            // Calculate aggregate value
            val aggregateValue = when (criterion.aggregateType?.uppercase()) {
                "COUNT" -> filteredEvents.size
                "DISTINCT_COUNT" -> {
                    val field = criterion.aggregateField ?: return false
                    filteredEvents.mapNotNull { event ->
                        event.eventDetails[field]?.toString()
                    }.distinct().size
                }

                else -> return false
            }

            // Compare with criterion values
            val targetValue = criterion.values?.firstOrNull()?.toIntOrNull() ?: return false

            return when (criterion.operator.uppercase()) {
                "EQUALS", "EQ" -> aggregateValue == targetValue
                "NOT_EQUALS", "NE" -> aggregateValue != targetValue
                "GREATER_THAN", "GT" -> aggregateValue > targetValue
                "GREATER_EQUAL", "GTE" -> aggregateValue >= targetValue
                "LESS_THAN", "LT" -> aggregateValue < targetValue
                "LESS_EQUAL", "LTE" -> aggregateValue <= targetValue
                else -> false
            }

        } catch (e: Exception) {
            DengageLogger.error("Error in operateEventHistoryFilter: ${e.message}")
            return false
        }
    }

    private fun parseTimeWindow(timeWindow: TimeWindow?): Long {
        try {
            val unit = timeWindow?.unit?.toLongOrNull() ?: return Long.MAX_VALUE

            return when (timeWindow.value.uppercase()) {
                "DAY" -> unit * 24 * 60 * 60 * 1000
                "HOUR" -> unit * 60 * 60 * 1000
                "MINUTE" -> unit * 60 * 1000
                "SECOND" -> unit * 1000
                else -> {
                    DengageLogger.error("Unknown time window value: ${timeWindow.value}")
                    Long.MAX_VALUE
                }
            }
        } catch (e: Exception) {
            DengageLogger.error("Error parsing new time window: ${e.message}")
            return Long.MAX_VALUE
        }
    }

    private fun applyEventFilters(
        events: List<ClientEvent>,
        filters: List<EventFilter>,
        logicalOp: String?
    ): List<ClientEvent> {
        if (filters.isEmpty()) return events

        return events.filter { event ->
            val filterResults = filters.map { filter ->
                applyEventFilter(event, filter)
            }

            when (logicalOp?.uppercase()) {
                "AND" -> filterResults.all { it }
                "OR" -> filterResults.any { it }
                else -> filterResults.all { it } // default to AND
            }
        }
    }

    private fun applyEventFilter(event: ClientEvent, filter: EventFilter): Boolean {
        val fieldValue = event.eventDetails[filter.field]?.toString() ?: return false

        return when (filter.op.uppercase()) {
            "EQUALS", "EQ" -> filter.values.contains(fieldValue)
            "NOT_EQUALS", "NE" -> !filter.values.contains(fieldValue)
            "IN" -> filter.values.contains(fieldValue)
            "NOT_IN" -> !filter.values.contains(fieldValue)
            "LIKE" -> filter.values.any { fieldValue.contains(it, ignoreCase = true) }
            "NOT_LIKE" -> !filter.values.any { fieldValue.contains(it, ignoreCase = true) }
            "STARTS_WITH" -> filter.values.any { fieldValue.startsWith(it, ignoreCase = true) }
            "NOT_STARTS_WITH" -> !filter.values.any { fieldValue.startsWith(it, ignoreCase = true) }
            "ENDS_WITH" -> filter.values.any { fieldValue.endsWith(it, ignoreCase = true) }
            "NOT_ENDS_WITH" -> !filter.values.any { fieldValue.endsWith(it, ignoreCase = true) }
            "GREATER_THAN", "GT" -> {
                val numFieldValue = fieldValue.toDoubleOrNull() ?: return false
                val numFilterValue = filter.values.firstOrNull()?.toDoubleOrNull() ?: return false
                numFieldValue > numFilterValue
            }

            "GREATER_EQUAL", "GTE" -> {
                val numFieldValue = fieldValue.toDoubleOrNull() ?: return false
                val numFilterValue = filter.values.firstOrNull()?.toDoubleOrNull() ?: return false
                numFieldValue >= numFilterValue
            }

            "LESS_THAN", "LT" -> {
                val numFieldValue = fieldValue.toDoubleOrNull() ?: return false
                val numFilterValue = filter.values.firstOrNull()?.toDoubleOrNull() ?: return false
                numFieldValue < numFilterValue
            }

            "LESS_EQUAL", "LTE" -> {
                val numFieldValue = fieldValue.toDoubleOrNull() ?: return false
                val numFilterValue = filter.values.firstOrNull()?.toDoubleOrNull() ?: return false
                numFieldValue <= numFilterValue
            }

            else -> false
        }
    }

}
