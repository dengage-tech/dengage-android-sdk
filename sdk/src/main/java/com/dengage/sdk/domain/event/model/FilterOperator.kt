package com.dengage.sdk.domain.event.model

enum class FilterOperator(val value: String) {
    EQUALS("equals"),
    NOT_EQUALS("not_equals"),
    BETWEEN("between"),
    NOT_BETWEEN("not_between"),
    IN("in"),
    NOT_IN("not_in"),
    AFTER("after"),
    AFTER_EQUAL("after_equal"),
    BEFORE("before"),
    BEFORE_EQUAL("before_equal"),
    NULL("null"),
    NOT_NULL("not_null"),
    LIKE("like"),
    NOT_LIKE("not_like"),
    STARTS_WITH("starts_with"),
    NOT_STARTS_WITH("not_starts_with"),
    ENDS_WITH("ends_with"),
    NOT_ENDS_WITH("not_ends_with"),
    GREATER_THAN("greater_than"),
    GREATER_EQUAL("greater_equal"),
    LESS_THAN("less_than"),
    LESS_EQUAL("less_equal"),
    EMPTY("empty"),
    NOT_EMPTY("not_empty");

    companion object {
        fun fromValue(value: String): FilterOperator? = values().find { it.value.lowercase() == value.lowercase() }
    }
}
