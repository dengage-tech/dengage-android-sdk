package com.dengage.sdk.manager.inappmessage.util

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.CartItem
import com.dengage.sdk.domain.inappmessage.model.Criterion
import com.dengage.sdk.domain.inappmessage.model.EventFilter
import com.dengage.sdk.util.DengageLogger

object CartUtils {

    fun operateCartFilter(criterion: Criterion): Boolean {
        try {
            val cart = Prefs.clientCart ?: return false

            val filteredItems = when {
                !criterion.filters.isNullOrEmpty() -> {
                    applyCartFilters(cart.items, criterion.filters, criterion.filtersLogicalOp)
                }

                else -> cart.items
            }

            val aggregateValue = when (criterion.aggregateType?.uppercase()) {
                "COUNT" -> filteredItems.size
                "DISTINCT_COUNT" -> {
                    val field = criterion.aggregateField ?: return false
                    getDistinctCount(filteredItems, field)
                }

                "SUM" -> {
                    val field = criterion.aggregateField ?: return false
                    getSumValue(filteredItems, field)
                }

                "MIN" -> {
                    val field = criterion.aggregateField ?: return false
                    getMinValue(filteredItems, field)
                }

                "MAX" -> {
                    val field = criterion.aggregateField ?: return false
                    getMaxValue(filteredItems, field)
                }

                "AVG" -> {
                    val field = criterion.aggregateField ?: return false
                    getAvgValue(filteredItems, field)
                }

                else -> return false
            }

            val targetValue = criterion.values?.firstOrNull()?.toDoubleOrNull() ?: return false
            val aggregateValueDouble = aggregateValue.toDouble()

            return when (criterion.operator.uppercase()) {
                "EQUALS", "EQ" -> aggregateValueDouble == targetValue
                "NOT_EQUALS", "NE" -> aggregateValueDouble != targetValue
                "GREATER_THAN", "GT" -> aggregateValueDouble > targetValue
                "GREATER_EQUAL", "GTE" -> aggregateValueDouble >= targetValue
                "LESS_THAN", "LT" -> aggregateValueDouble < targetValue
                "LESS_EQUAL", "LTE" -> aggregateValueDouble <= targetValue
                else -> false
            }

        } catch (e: Exception) {
            DengageLogger.error("Error in operateCartFilter: ${e.message}")
            return false
        }
    }

    private fun applyCartFilters(
        items: List<CartItem>,
        filters: List<EventFilter>,
        logicalOp: String?
    ): List<CartItem> {
        if (filters.isEmpty()) return items

        return items.filter { item ->
            val filterResults = filters.map { filter ->
                applyCartFilter(item, filter)
            }

            when (logicalOp?.uppercase()) {
                "AND" -> filterResults.all { it }
                "OR" -> filterResults.any { it }
                else -> filterResults.all { it } // default to AND
            }
        }
    }

    private fun applyCartFilter(item: CartItem, filter: EventFilter): Boolean {
        val fieldValue = getFieldValue(item, filter.parameter) ?: return false

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
            "CONTAINS_ALL" -> {
                // Special case for category_path - check if all values are present in the path
                if (filter.parameter == "category_path") {
                    filter.values.all { value ->
                        fieldValue.contains(value, ignoreCase = true)
                    }
                } else {
                    filter.values.all { value ->
                        fieldValue.contains(value, ignoreCase = true)
                    }
                }
            }

            "CONTAINS_ANY" -> {
                filter.values.any { value ->
                    fieldValue.contains(value, ignoreCase = true)
                }
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
            "BETWEEN" -> {
                when (filter.dataType.uppercase()) {
                    "INT" -> {
                        val numFieldValue = fieldValue.toIntOrNull() ?: return false
                        if (filter.values.size >= 2) {
                            val firstValue = filter.values[0].toIntOrNull() ?: return false
                            val lastValue = filter.values[1].toIntOrNull() ?: return false
                            val minValue = minOf(firstValue, lastValue)
                            val maxValue = maxOf(firstValue, lastValue)
                            numFieldValue in (minValue + 1) until maxValue
                        } else false
                    }
                    "TEXT" -> {
                        val numFieldValue = fieldValue.toDoubleOrNull() ?: return false
                        if (filter.values.size >= 2) {
                            val firstValue = filter.values[0].toDoubleOrNull() ?: return false
                            val lastValue = filter.values[1].toDoubleOrNull() ?: return false
                            val minValue = minOf(firstValue, lastValue)
                            val maxValue = maxOf(firstValue, lastValue)
                            numFieldValue > minValue && numFieldValue < maxValue
                        } else false
                    }
                    else -> false
                }
            }
            "NOT_BETWEEN" -> {
                when (filter.dataType.uppercase()) {
                    "INT" -> {
                        val numFieldValue = fieldValue.toIntOrNull() ?: return false
                        if (filter.values.size >= 2) {
                            val firstValue = filter.values[0].toIntOrNull() ?: return false
                            val lastValue = filter.values[1].toIntOrNull() ?: return false
                            val minValue = minOf(firstValue, lastValue)
                            val maxValue = maxOf(firstValue, lastValue)
                            numFieldValue !in (minValue + 1) until maxValue
                        } else true
                    }
                    "TEXT" -> {
                        val numFieldValue = fieldValue.toDoubleOrNull() ?: return false
                        if (filter.values.size >= 2) {
                            val firstValue = filter.values[0].toDoubleOrNull() ?: return false
                            val lastValue = filter.values[1].toDoubleOrNull() ?: return false
                            val minValue = minOf(firstValue, lastValue)
                            val maxValue = maxOf(firstValue, lastValue)
                            !(numFieldValue > minValue && numFieldValue < maxValue)
                        } else true
                    }
                    else -> true
                }
            }
            else -> false
        }
    }

    private fun getFieldValue(item: CartItem, field: String): String? {
        return when (field) {
            "product_id" -> item.productId
            "product_variant_id" -> item.productVariantId
            "category_path" -> item.categoryPath
            "price" -> item.price.toString()
            "discounted_price" -> item.discountedPrice.toString()
            "has_discount" -> item.hasDiscount.toString()
            "has_promotion" -> item.hasPromotion.toString()
            "quantity" -> item.quantity.toString()
            "effective_price" -> item.effectivePrice.toString()
            "line_total" -> item.lineTotal.toString()
            "discounted_line_total" -> item.discountedLineTotal.toString()
            "effective_line_total" -> item.effectiveLineTotal.toString()
            "category_root" -> item.categoryRoot
            else -> {
                // Handle attributes with dot notation (e.g., "attributes.brand")
                if (field.startsWith("attributes.")) {
                    val attributeKey = field.removePrefix("attributes.")
                    item.attributes[attributeKey]
                } else {
                    item.attributes[field]
                }
            }
        }
    }

    private fun getDistinctCount(items: List<CartItem>, field: String): Double {
        val distinctValues = items.mapNotNull { item ->
            getFieldValue(item, field)
        }.distinct()
        return distinctValues.size.toDouble()
    }

    private fun getSumValue(items: List<CartItem>, field: String): Double {
        return items.sumOf { item ->
            when (field) {
                "price" -> item.price.toDouble()
                "discounted_price" -> item.discountedPrice.toDouble()
                "quantity" -> item.quantity.toDouble()
                "effective_price" -> item.effectivePrice.toDouble()
                "line_total" -> item.lineTotal.toDouble()
                "discounted_line_total" -> item.discountedLineTotal.toDouble()
                "effective_line_total" -> item.effectiveLineTotal.toDouble()
                else -> {
                    val value = getFieldValue(item, field)
                    value?.toDoubleOrNull() ?: 0.0
                }
            }
        }
    }

    private fun getMinValue(items: List<CartItem>, field: String): Double {
        val values = items.mapNotNull { item ->
            when (field) {
                "price" -> item.price.toDouble()
                "discounted_price" -> item.discountedPrice.toDouble()
                "quantity" -> item.quantity.toDouble()
                "effective_price" -> item.effectivePrice.toDouble()
                "line_total" -> item.lineTotal.toDouble()
                "discounted_line_total" -> item.discountedLineTotal.toDouble()
                "effective_line_total" -> item.effectiveLineTotal.toDouble()
                else -> {
                    val value = getFieldValue(item, field)
                    value?.toDoubleOrNull()
                }
            }
        }
        return values.minOrNull() ?: 0.0
    }

    private fun getMaxValue(items: List<CartItem>, field: String): Double {
        val values = items.mapNotNull { item ->
            when (field) {
                "price" -> item.price.toDouble()
                "discounted_price" -> item.discountedPrice.toDouble()
                "quantity" -> item.quantity.toDouble()
                "effective_price" -> item.effectivePrice.toDouble()
                "line_total" -> item.lineTotal.toDouble()
                "discounted_line_total" -> item.discountedLineTotal.toDouble()
                "effective_line_total" -> item.effectiveLineTotal.toDouble()
                else -> {
                    val value = getFieldValue(item, field)
                    value?.toDoubleOrNull()
                }
            }
        }
        return values.maxOrNull() ?: 0.0
    }

    private fun getAvgValue(items: List<CartItem>, field: String): Double {
        if (items.isEmpty()) return 0.0
        val sum = getSumValue(items, field)
        return sum / items.size
    }
}
