package com.dengage.sdk.domain.geofence.usecase

import retrofit2.Response
import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.geofence.GeofenceRepository
import com.dengage.sdk.util.createLazy

class SendGeofenceEventSignal : CoroutineUseCase<Response<Unit>, SendGeofenceEventSignal.Params>() {

    private val repository: GeofenceRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.sendGeofenceEventSignal(
            integrationKey = params!!.integrationKey,
            clusterId = params.clusterId,
            geofenceId = params.geofenceId,
            deviceId = params.deviceId,
            contactKey = params.contactKey,
            latitude = params.latitude,
            longitude = params.longitude,
            type = params.type,
            token = params.token,
            permit = params.permit
        )

    data class Params(
        val integrationKey: String,
        val clusterId: Int,
        val geofenceId: Int,
        val deviceId: String,
        val contactKey: String,
        val latitude: Double,
        val longitude: Double,
        val type: String,
        val token: String,
        val permit: Boolean
    )
}
