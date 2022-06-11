package com.dengage.sdk.domain.geofence.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class GeofenceCluster(
    @SerializedName("id") val id: Int,
    @SerializedName("geofences") val geofences: Array<GeofenceItem>
) : Serializable {

    data class GeofenceItem(
        @SerializedName("id") val id: Int,
        @SerializedName("lat") val latitude: Double,
        @SerializedName("long") val longitude: Double,
        @SerializedName("radius") val radius: Double
    ) : Serializable
}