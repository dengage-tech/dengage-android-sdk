package com.dengage.sdk.domain.geofence

import retrofit2.Response
import com.dengage.sdk.data.remote.api.ApiType
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.geofence.model.GeofenceCluster
import com.dengage.sdk.domain.geofence.model.GeofenceEventSignalRequest


class GeofenceRepository {

    private val service: GeofenceService by service(ApiType.GEOFENCE)

    suspend fun getGeofences(
        integrationKey: String,
        latitude: Double,
        longitude: Double
    ): Array<GeofenceCluster>? {
        return service.getGeofences(
            integrationKey = integrationKey,
            latitude = latitude,
            longitude = longitude
        )
    }

    suspend fun sendGeofenceEventSignal(
        integrationKey: String,
        clusterId: Int,
        geofenceId: Int,
        deviceId: String,
        contactKey: String,
        latitude: Double,
        longitude: Double,
        type: String,
        token: String,
        permit: Boolean
    ): Response<Unit> {
        return service.sendGeofenceEventSignal(
            integrationKey = integrationKey,
            request = GeofenceEventSignalRequest(
                clusterId = clusterId,
                geofenceId = geofenceId,
                deviceId = deviceId,
                contactKey = contactKey,
                latitude = latitude,
                longitude = longitude,
                type = type,
                token = token,
                permit = permit
            )

        )
    }

}