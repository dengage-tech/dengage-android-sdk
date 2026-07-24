package com.dengage.sdk.domain.geofence.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.geofence.GeofenceRepository
import com.dengage.sdk.domain.geofence.model.sync.GeofenceSyncResponse
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class GetGeofenceSync : CoroutineUseCase<Response<GeofenceSyncResponse>, GetGeofenceSync.Params>() {

    private val repository: GeofenceRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<GeofenceSyncResponse> =
        repository.getGeofenceSync(
            integrationKey = params!!.integrationKey,
            deviceId = params.deviceId,
            contactKey = params.contactKey,
            latitude = params.latitude,
            longitude = params.longitude,
            ifNoneMatch = params.ifNoneMatch
        )

    data class Params(
        val integrationKey: String,
        val deviceId: String?,
        val contactKey: String?,
        val latitude: Double?,
        val longitude: Double?,
        val ifNoneMatch: String?
    )
}
