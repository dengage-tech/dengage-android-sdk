package com.dengage.sdk.domain.geofence.model.sync

import com.google.gson.annotations.SerializedName

/**
 * Kampanya tetikleme türü. Wire değeri küçük harf string (contract §0).
 */
enum class GeofenceTriggerType {
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

    companion object {
        fun fromWire(value: String?): GeofenceTriggerType = when (value?.lowercase()) {
            "exit" -> EXIT
            "dwell" -> DWELL
            else -> ENTER
        }
    }
}
