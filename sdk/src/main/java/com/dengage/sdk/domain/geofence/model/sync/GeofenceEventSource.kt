package com.dengage.sdk.domain.geofence.model.sync

import com.google.gson.annotations.SerializedName

/**
 * event-signal v2 source alanı. `online` = anlık tetik, `replay` = offline kuyruktan flush (contract §3).
 */
enum class GeofenceEventSource {
    @SerializedName("online")
    ONLINE,

    @SerializedName("replay")
    REPLAY;

    val wireValue: String
        get() = when (this) {
            ONLINE -> "online"
            REPLAY -> "replay"
        }
}
