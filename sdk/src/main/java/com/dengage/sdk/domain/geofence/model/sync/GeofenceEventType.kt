package com.dengage.sdk.domain.geofence.model.sync

import com.google.gson.annotations.SerializedName

/**
 * event-signal v2 eventType alanı. Wire değeri küçük harf string (contract §3).
 */
enum class GeofenceEventType {
    @SerializedName("enter")
    ENTER,

    @SerializedName("exit")
    EXIT,

    @SerializedName("dwell")
    DWELL;

    val wireValue: String
        get() = when (this) {
            ENTER -> "enter"
            EXIT -> "exit"
            DWELL -> "dwell"
        }
}
