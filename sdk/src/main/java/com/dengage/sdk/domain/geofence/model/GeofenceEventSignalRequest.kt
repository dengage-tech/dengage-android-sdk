package com.dengage.sdk.domain.geofence.model

import com.dengage.sdk.util.Constants
import com.google.gson.annotations.SerializedName
import java.io.Serializable
import java.text.SimpleDateFormat
import java.util.*

data class GeofenceEventSignalRequest(
    @SerializedName("cid") val clusterId: Int,
    @SerializedName("geoid") val geofenceId: Int,
    @SerializedName("did") val deviceId: String,
    @SerializedName("ckey") val contactKey: String,
    @SerializedName("et") var eventTime: String,
    @SerializedName("loc") var location: Location,
    @SerializedName("type") var type: String,
    @SerializedName("token") var token: String,
    @SerializedName("permit") var permit: Boolean,
) : Serializable {

    constructor(
        clusterId: Int,
        geofenceId: Int,
        deviceId: String,
        contactKey: String,
        latitude: Double,
        longitude: Double,
        type: String,
        token: String,
        permit: Boolean
    ) : this(
        clusterId,
        geofenceId,
        deviceId,
        contactKey,
        SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault()).format(Date()),
        Location(latitude, longitude),
        type,
        token,
        permit
    )

    data class Location(
        @SerializedName("lat") val latitude: Double,
        @SerializedName("long") val longitude: Double
    ) : Serializable

}

