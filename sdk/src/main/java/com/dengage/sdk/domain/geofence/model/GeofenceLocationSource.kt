package com.dengage.sdk.domain.geofence.model

enum class GeofenceLocationSource {
    FOREGROUND_LOCATION,
    BACKGROUND_LOCATION,
    MANUAL_LOCATION,
    GEOFENCE_ENTER,
    GEOFENCE_DWELL,
    GEOFENCE_EXIT,
    MOCK_LOCATION,
    UNKNOWN;

    fun getStringValue(): String {
        return when (this) {
            FOREGROUND_LOCATION -> "FOREGROUND_LOCATION"
            BACKGROUND_LOCATION -> "BACKGROUND_LOCATION"
            MANUAL_LOCATION -> "MANUAL_LOCATION"
            GEOFENCE_ENTER -> "GEOFENCE_ENTER"
            GEOFENCE_DWELL -> "GEOFENCE_DWELL"
            GEOFENCE_EXIT -> "GEOFENCE_EXIT"
            MOCK_LOCATION -> "MOCK_LOCATION"
            UNKNOWN -> "UNKNOWN"
        }
    }
}