package com.dengage.sdk.domain.geofence.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable
import java.util.Date

class GeofenceHistory(
    @SerializedName("lastLat") var lastLat: Double = 0.0,
    @SerializedName("lastLon") var lastLon: Double = 0.0,
    @SerializedName("lastFetchTime") var lastFetchTime: Long = 0,
    @SerializedName("fetchHistory") var fetchHistory: HashMap<Long, Array<GeofenceCluster>> = hashMapOf<Long, Array<GeofenceCluster>>(),
    @SerializedName("eventHistory") var eventHistory: HashMap<String, HashMap<Long, GeofenceEvent>> = hashMapOf<String, HashMap<Long, GeofenceEvent>>()
) : Serializable