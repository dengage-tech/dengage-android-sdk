package com.dengage.sdk.manager.inappmessage.util

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.event.model.ClientEvent
import com.dengage.sdk.domain.inappmessage.model.Criterion
import com.dengage.sdk.domain.inappmessage.model.EventFilter
import com.dengage.sdk.domain.inappmessage.model.TimeWindow
import com.dengage.sdk.manager.event.EventKey
import com.dengage.sdk.manager.session.SessionManager
import com.dengage.sdk.util.DengageLogger

object EventHistoryUtils {

    fun operateEventHistoryFilter(criterion: Criterion): Boolean {
        try {
            val eventType = criterion.eventType ?: return false

            val clientEvents = Prefs.clientEvents
            val eventTypeEvents = clientEvents[eventType] ?: return false

            // Filter events by time window
            val eventsInWindow = if (criterion.timeWindow?.type?.uppercase() == "SESSION") {
                // For SESSION type, filter events that belong to current session
                val currentSessionId = SessionManager.getSessionId()
                eventTypeEvents.filter { event ->
                    event.sessionId == currentSessionId
                }
            } else {
                // For other time window types, filter by time
                val windowMillis = parseTimeWindow(criterion.timeWindow)
                val cutoffTime = System.currentTimeMillis() - windowMillis
                eventTypeEvents.filter { it.timestamp >= cutoffTime }
            }

            // Apply additional filters if any
            val filteredEvents = when {
                !criterion.filters.isNullOrEmpty() -> {
                    applyEventFilters(eventsInWindow, criterion.filters, criterion.filtersLogicalOp)
                }

                else -> eventsInWindow
            }

            // Calculate aggregate value
            val aggregateValue = when (criterion.aggregateType?.uppercase()) {
                "COUNT" -> filteredEvents.size
                "SUM" -> {
                    val field = criterion.aggregateField ?: return false
                    filteredEvents.sumOf { event ->
                        event.eventDetails[field]?.toString()?.toDoubleOrNull() ?: 0.0
                    }
                }

                "MIN" -> {
                    val field = criterion.aggregateField ?: return false
                    filteredEvents.mapNotNull { event ->
                        event.eventDetails[field]?.toString()?.toDoubleOrNull()
                    }.minOrNull() ?: return false
                }

                "MAX" -> {
                    val field = criterion.aggregateField ?: return false
                    filteredEvents.mapNotNull { event ->
                        event.eventDetails[field]?.toString()?.toDoubleOrNull()
                    }.maxOrNull() ?: return false
                }

                "AVG" -> {
                    val field = criterion.aggregateField ?: return false
                    val values = filteredEvents.mapNotNull { event ->
                        event.eventDetails[field]?.toString()?.toDoubleOrNull()
                    }
                    if (values.isEmpty()) return false
                    values.average()
                }

                "DISTINCT_COUNT" -> {
                    val field = criterion.aggregateField ?: return false
                    filteredEvents.mapNotNull { event ->
                        event.eventDetails[field]?.toString()
                    }.distinct().size
                }

                else -> return false
            }

            // Compare with criterion values
            val targetValue = criterion.values?.firstOrNull()?.toDoubleOrNull() ?: return false

            return when (criterion.operator.uppercase()) {
                "EQUALS", "EQ" -> aggregateValue.toDouble() == targetValue
                "NOT_EQUALS", "NE" -> aggregateValue.toDouble() != targetValue
                "GREATER_THAN", "GT" -> aggregateValue.toDouble() > targetValue
                "GREATER_EQUAL", "GTE" -> aggregateValue.toDouble() >= targetValue
                "LESS_THAN", "LT" -> aggregateValue.toDouble() < targetValue
                "LESS_EQUAL", "LTE" -> aggregateValue.toDouble() <= targetValue
                else -> false
            }

        } catch (e: Exception) {
            DengageLogger.error("Error in operateEventHistoryFilter: ${e.message}")
            return false
        }
    }

    private fun parseTimeWindow(timeWindow: TimeWindow?): Long {
        return when (timeWindow?.unit?.uppercase()) {
            "MINUTE" -> (timeWindow.value.toLongOrNull() ?: 30) * 60 * 1000
            "HOUR" -> (timeWindow.value.toLongOrNull() ?: 1) * 60 * 60 * 1000
            "DAY" -> (timeWindow.value.toLongOrNull() ?: 1) * 24 * 60 * 60 * 1000
            "WEEK" -> (timeWindow.value.toLongOrNull() ?: 1) * 7 * 24 * 60 * 60 * 1000
            "MONTH" -> (timeWindow.value.toLongOrNull() ?: 1) * 30 * 24 * 60 * 60 * 1000
            "SESSION" -> 0L // Return 0 for SESSION type as it's handled separately
            else -> 30 * 60 * 1000 // Default to 30 minutes
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
        val fieldValue = event.eventDetails[filter.parameter]?.toString() ?: return false

        return when (filter.comparison.uppercase()) {
            "EQUALS", "EQ" -> {
                when (filter.dataType.uppercase()) {
                    "BOOL" -> {
                        val boolFieldValue =
                            fieldValue.toBooleanStrictOrNull() ?: fieldValue.equals(
                                "true",
                                ignoreCase = true
                            )
                        filter.values.any { value ->
                            val boolFilterValue = value.toBooleanStrictOrNull() ?: value.equals(
                                "true",
                                ignoreCase = true
                            )
                            boolFieldValue == boolFilterValue
                        }
                    }

                    else -> filter.values.any { it.equals(fieldValue, ignoreCase = true) }
                }
            }

            "NOT_EQUALS", "NE" -> {
                when (filter.dataType.uppercase()) {
                    "BOOL" -> {
                        val boolFieldValue =
                            fieldValue.toBooleanStrictOrNull() ?: fieldValue.equals(
                                "true",
                                ignoreCase = true
                            )
                        !filter.values.any { value ->
                            val boolFilterValue = value.toBooleanStrictOrNull() ?: value.equals(
                                "true",
                                ignoreCase = true
                            )
                            boolFieldValue == boolFilterValue
                        }
                    }

                    else -> !filter.values.any { it.equals(fieldValue, ignoreCase = true) }
                }
            }

            "IN" -> {
                when (filter.dataType.uppercase()) {
                    "BOOL" -> {
                        val boolFieldValue =
                            fieldValue.toBooleanStrictOrNull() ?: fieldValue.equals(
                                "true",
                                ignoreCase = true
                            )
                        filter.values.any { value ->
                            val boolFilterValue = value.toBooleanStrictOrNull() ?: value.equals(
                                "true",
                                ignoreCase = true
                            )
                            boolFieldValue == boolFilterValue
                        }
                    }

                    else -> filter.values.any { it.equals(fieldValue, ignoreCase = true) }
                }
            }

            "NOT_IN" -> {
                when (filter.dataType.uppercase()) {
                    "BOOL" -> {
                        val boolFieldValue =
                            fieldValue.toBooleanStrictOrNull() ?: fieldValue.equals(
                                "true",
                                ignoreCase = true
                            )
                        !filter.values.any { value ->
                            val boolFilterValue = value.toBooleanStrictOrNull() ?: value.equals(
                                "true",
                                ignoreCase = true
                            )
                            boolFieldValue == boolFilterValue
                        }
                    }

                    else -> !filter.values.any { it.equals(fieldValue, ignoreCase = true) }
                }
            }

            "LIKE" -> filter.values.any { fieldValue.contains(it, ignoreCase = true) }
            "NOT_LIKE" -> !filter.values.any { fieldValue.contains(it, ignoreCase = true) }
            "STARTS_WITH" -> filter.values.any { fieldValue.startsWith(it, ignoreCase = true) }
            "NOT_STARTS_WITH" -> !filter.values.any { fieldValue.startsWith(it, ignoreCase = true) }
            "ENDS_WITH" -> filter.values.any { fieldValue.endsWith(it, ignoreCase = true) }
            "NOT_ENDS_WITH" -> !filter.values.any { fieldValue.endsWith(it, ignoreCase = true) }
            "CONTAINS" -> filter.values.any { fieldValue.contains(it, ignoreCase = true) }
            "NOT_CONTAINS" -> !filter.values.any { fieldValue.contains(it, ignoreCase = true) }
            "CONTAINS_ALL" -> filter.values.all { value ->
                fieldValue.contains(value, ignoreCase = true)
            }

            "CONTAINS_ANY" -> filter.values.any { value ->
                fieldValue.contains(value, ignoreCase = true)
            }

            "GREATER_THAN", "GT" -> {
                when (filter.dataType.uppercase()) {
                    "INT" -> {
                        val numFieldValue = fieldValue.toIntOrNull() ?: return false
                        val numFilterValue =
                            filter.values.firstOrNull()?.toIntOrNull() ?: return false
                        numFieldValue > numFilterValue
                    }

                    "TEXT" -> {
                        val numFieldValue = fieldValue.toDoubleOrNull() ?: return false
                        val numFilterValue =
                            filter.values.firstOrNull()?.toDoubleOrNull() ?: return false
                        numFieldValue > numFilterValue
                    }

                    "BOOL" -> {
                        // For boolean: true > false (true = 1, false = 0)
                        val boolFieldValue =
                            fieldValue.toBooleanStrictOrNull() ?: fieldValue.equals(
                                "true",
                                ignoreCase = true
                            )
                        val boolFilterValue = filter.values.firstOrNull()?.let { value ->
                            value.toBooleanStrictOrNull() ?: value.equals("true", ignoreCase = true)
                        } ?: return false
                        val fieldInt = if (boolFieldValue) 1 else 0
                        val filterInt = if (boolFilterValue) 1 else 0
                        fieldInt > filterInt
                    }

                    else -> false
                }
            }

            "GREATER_EQUAL", "GTE" -> {
                when (filter.dataType.uppercase()) {
                    "INT" -> {
                        val numFieldValue = fieldValue.toIntOrNull() ?: return false
                        val numFilterValue =
                            filter.values.firstOrNull()?.toIntOrNull() ?: return false
                        numFieldValue >= numFilterValue
                    }

                    "TEXT" -> {
                        val numFieldValue = fieldValue.toDoubleOrNull() ?: return false
                        val numFilterValue =
                            filter.values.firstOrNull()?.toDoubleOrNull() ?: return false
                        numFieldValue >= numFilterValue
                    }

                    "BOOL" -> {
                        val boolFieldValue =
                            fieldValue.toBooleanStrictOrNull() ?: fieldValue.equals(
                                "true",
                                ignoreCase = true
                            )
                        val boolFilterValue = filter.values.firstOrNull()?.let { value ->
                            value.toBooleanStrictOrNull() ?: value.equals("true", ignoreCase = true)
                        } ?: return false
                        val fieldInt = if (boolFieldValue) 1 else 0
                        val filterInt = if (boolFilterValue) 1 else 0
                        fieldInt >= filterInt
                    }

                    else -> false
                }
            }

            "LESS_THAN", "LT" -> {
                when (filter.dataType.uppercase()) {
                    "INT" -> {
                        val numFieldValue = fieldValue.toIntOrNull() ?: return false
                        val numFilterValue =
                            filter.values.firstOrNull()?.toIntOrNull() ?: return false
                        numFieldValue < numFilterValue
                    }

                    "TEXT" -> {
                        val numFieldValue = fieldValue.toDoubleOrNull() ?: return false
                        val numFilterValue =
                            filter.values.firstOrNull()?.toDoubleOrNull() ?: return false
                        numFieldValue < numFilterValue
                    }

                    "BOOL" -> {
                        val boolFieldValue =
                            fieldValue.toBooleanStrictOrNull() ?: fieldValue.equals(
                                "true",
                                ignoreCase = true
                            )
                        val boolFilterValue = filter.values.firstOrNull()?.let { value ->
                            value.toBooleanStrictOrNull() ?: value.equals("true", ignoreCase = true)
                        } ?: return false
                        val fieldInt = if (boolFieldValue) 1 else 0
                        val filterInt = if (boolFilterValue) 1 else 0
                        fieldInt < filterInt
                    }

                    else -> false
                }
            }

            "LESS_EQUAL", "LTE" -> {
                when (filter.dataType.uppercase()) {
                    "INT" -> {
                        val numFieldValue = fieldValue.toIntOrNull() ?: return false
                        val numFilterValue =
                            filter.values.firstOrNull()?.toIntOrNull() ?: return false
                        numFieldValue <= numFilterValue
                    }

                    "TEXT" -> {
                        val numFieldValue = fieldValue.toDoubleOrNull() ?: return false
                        val numFilterValue =
                            filter.values.firstOrNull()?.toDoubleOrNull() ?: return false
                        numFieldValue <= numFilterValue
                    }

                    "BOOL" -> {
                        val boolFieldValue =
                            fieldValue.toBooleanStrictOrNull() ?: fieldValue.equals(
                                "true",
                                ignoreCase = true
                            )
                        val boolFilterValue = filter.values.firstOrNull()?.let { value ->
                            value.toBooleanStrictOrNull() ?: value.equals("true", ignoreCase = true)
                        } ?: return false
                        val fieldInt = if (boolFieldValue) 1 else 0
                        val filterInt = if (boolFilterValue) 1 else 0
                        fieldInt <= filterInt
                    }

                    else -> false
                }
            }

            "EXISTS" -> fieldValue.isNotEmpty()
            "NOT_EXISTS" -> fieldValue.isEmpty()
            else -> false
        }
    }

}

