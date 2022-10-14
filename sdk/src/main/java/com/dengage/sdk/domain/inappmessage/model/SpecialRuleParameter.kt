package com.dengage.sdk.domain.inappmessage.model

enum class SpecialRuleParameter(val key: String) {
    CATEGORY_PATH("dn.cat_path"),
    CART_ITEM_COUNT("dn.cart_item_count"),
    CART_AMOUNT("dn.cart_amount"),
    STATE("dn.state"),
    CITY("dn.city"),
    TIMEZONE("dn.tz"),
    LANGUAGE("dn.lang"),
    SCREEN_WIDTH("dn.sc_width"),
    SCREEN_HEIGHT("dn.sc_height"),
    OS_VERSION("dn.os_ver"),
    OS("dn.os"),
    DEVICE_NAME("dn.device_name"),
    COUNTRY("dn.country"),
    MONTH("dn.month"),
    WEEK_DAY("dn.week_day"),
    HOUR("dn.hour"),
    PAGE_VIEW_IN_VISIT("dn.pviv"),
    ANONYMOUS("dn.anonym"),
    VISIT_DURATION("dn.visit_duration"),
    FIRST_VISIT("dn.first_visit"),
    LAST_VISIT("dn.last_visit_ts"),
    BRAND_NAME("dn.brand_name"),
    MODEL_NAME("dn.model_name"),
    PUSH_PERMISSION("dn.wp_perm"),
    VISIT_COUNT("dn.visit_count")
}