package com.dengage.sdk.data.cache

enum class PreferenceKey {
    ___DEN_DEVICE_UNIQUE_ID___,
    PUSH_API_BASE_URL,
    EVENT_API_BASE_URL,
    IN_APP_MESSAGES,
    SDK_PARAMETERS,
    IN_APP_MESSAGE_FETCH_TIME,
    APP_TRACKING_TIME,
    IN_APP_MESSAGE_SHOW_TIME,
    NOTIFICATION_CHANNEL_NAME,
    SUBSCRIPTION,
    INBOX_MESSAGE_FETCH_TIME,
    APP_SESSION_TIME,
    APP_SESSION_ID,
    LOG_VISIBILITY,
    RFM_SCORES,
    GEOFENCE_API_BASE_URL,
    GEOFENCE_ENABLED,
    GEOFENCE_HISTORY,
    GEOFENCE_PERMISSIONS_DENIED;

    override fun toString(): String = name
}