package com.dengage.sdk.manager.inappmessage.util

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.event.model.ClientEvent
import com.dengage.sdk.domain.inappmessage.model.Criterion
import com.dengage.sdk.domain.inappmessage.model.Filter
import com.dengage.sdk.util.DengageLogger

object EventHistoryUtils {

    fun operateEventHistoryFilter(criterion: Criterion): Boolean {
        try {
            val eventType = criterion.event ?: return false
            val clientEvents = Prefs.clientEvents
            val eventTypeEvents = clientEvents[eventType] ?: return false

            // Parse window (e.g., "P7D" = 7 days)
            val windowMillis = parseTimeWindow(criterion.window)
            val cutoffTime = System.currentTimeMillis() - windowMillis

            // Filter events by time window
            val eventsInWindow = eventTypeEvents.filter { it.timestamp >= cutoffTime }

            // Apply filters if present
            val filteredEvents = if (!criterion.filters.isNullOrEmpty()) {
                applyEventFilters(eventsInWindow, criterion.filters, criterion.filtersLogicalOp)
            } else {
                eventsInWindow
            }

            // Calculate aggregate value
            val aggregateValue = when (criterion.aggregateType) {
                "count" -> filteredEvents.size
                "distinct_count" -> {
                    val field = criterion.field ?: return false
                    filteredEvents.mapNotNull { event ->
                        event.eventDetails[field]?.toString()
                    }.distinct().size
                }
                else -> return false
            }

            // Compare with criterion values
            val targetValue = criterion.values?.firstOrNull()?.toIntOrNull() ?: return false

            return when (criterion.operator) {
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

    private fun parseTimeWindow(window: String?): Long {
        if (window.isNullOrEmpty()) return Long.MAX_VALUE

        try {
            // Parse ISO 8601 duration format (e.g., P7D, PT24H, P30M)
            val regex = Regex("P(?:(\\d+)D)?(?:T(?:(\\d+)H)?(?:(\\d+)M)?(?:(\\d+)S)?)?")
            val matchResult = regex.find(window) ?: return Long.MAX_VALUE

            val days = matchResult.groupValues[1].toLongOrNull() ?: 0
            val hours = matchResult.groupValues[2].toLongOrNull() ?: 0
            val minutes = matchResult.groupValues[3].toLongOrNull() ?: 0
            val seconds = matchResult.groupValues[4].toLongOrNull() ?: 0

            return (days * 24 * 60 * 60 * 1000) +
                    (hours * 60 * 60 * 1000) +
                    (minutes * 60 * 1000) +
                    (seconds * 1000)
        } catch (e: Exception) {
            DengageLogger.error("Error parsing time window: ${e.message}")
            return Long.MAX_VALUE
        }
    }

    private fun applyEventFilters(
        events: List<ClientEvent>,
        filters: List<Filter>,
        logicalOp: String?
    ): List<ClientEvent> {
        if (filters.isEmpty()) return events

        return events.filter { event ->
            val filterResults = filters.map { filter ->
                applyEventFilter(event, filter)
            }

            when (logicalOp) {
                "AND" -> filterResults.all { it }
                "OR" -> filterResults.any { it }
                else -> filterResults.all { it } // default to AND
            }
        }
    }

    private fun applyEventFilter(event: ClientEvent, filter: Filter): Boolean {
        val fieldValue = event.eventDetails[filter.field]?.toString() ?: return false

        return when (filter.op) {
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
