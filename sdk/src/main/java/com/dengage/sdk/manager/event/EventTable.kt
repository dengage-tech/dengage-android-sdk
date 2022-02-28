package com.dengage.sdk.manager.event

enum class EventTable(val tableName: String) {
    WISHLIST_EVENTS("wishlist_events"),
    WISHLIST_EVENTS_DETAIL("wishlist_events_detail"),
    SEARCH_EVENTS("search_events"),
    ORDER_EVENTS("order_events"),
    SHOPPING_CART_EVENTS("shopping_cart_events"),
    SHOPPING_CART_EVENTS_DETAIL("shopping_cart_events_detail"),
    ORDER_EVENTS_DETAIL("order_events_detail"),
    PAGE_VIEW_EVENTS("page_view_events"),
    SESSION_INFO("session_info"),
}