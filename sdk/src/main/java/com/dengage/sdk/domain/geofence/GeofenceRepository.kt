package com.dengage.sdk.domain.geofence

import retrofit2.Response
import com.dengage.sdk.data.remote.api.ApiType
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.geofence.model.GeofenceCluster
import com.dengage.sdk.domain.geofence.model.GeofenceEventSignalRequest
import com.dengage.sdk.domain.geofence.model.sync.DeviceHeartbeatRequest
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventSignalRequestV2
import com.dengage.sdk.domain.geofence.model.sync.GeofenceSyncResponse


class GeofenceRepository {

    private val service: GeofenceService by service(ApiType.GEOFENCE)

    /** v2 sync. ETag header + 304 ayırt edebilmek için ham [Response] döner (contract §1). */
    suspend fun getGeofenceSync(
        integrationKey: String,
        deviceId: String?,
        contactKey: String?,
        latitude: Double?,
        longitude: Double?,
        ifNoneMatch: String?
    ): Response<GeofenceSyncResponse> {
        return service.getGeofenceSync(
            integrationKey = integrationKey,
            deviceId = deviceId,
            contactKey = contactKey,
            lat = latitude,
            lon = longitude,
            ifNoneMatch = ifNoneMatch
        )
    }

    suspend fun sendDeviceHeartbeat(
        integrationKey: String,
        request: DeviceHeartbeatRequest
    ): Response<Unit> {
        return service.sendDeviceHeartbeat(integrationKey = integrationKey, request = request)
    }

    suspend fun sendGeofenceEventSignalV2(
        integrationKey: String,
        request: GeofenceEventSignalRequestV2
    ): Response<Unit> {
        return service.sendGeofenceEventSignalV2(integrationKey = integrationKey, request = request)
    }

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