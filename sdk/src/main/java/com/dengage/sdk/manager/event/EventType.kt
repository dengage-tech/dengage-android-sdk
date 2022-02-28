package com.dengage.sdk.manager.event

enum class EventType(val type: String) {
    ADD("add"),
    REMOVE("remove"),
    ORDER("order"),
    CANCEL("cancel"),
    ADD_TO_CART("add_to_cart"),
    REMOVE_FROM_CART("remove_from_cart"),
    VIEW_CART("view_cart"),
    BEGIN_CHECKOUT("begin_checkout"),
}