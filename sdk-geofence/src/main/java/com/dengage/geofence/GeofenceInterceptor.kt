package com.dengage.geofence

interface GeofenceInterceptor {
    fun onGeofenceEnter(
        latitude: Double,
        longitude: Double,
        radius: Double,
        clusterId: Int,
        clusterName: String?,
        geofenceItemId: Int,
        geofenceItemName: String?
    )
}
