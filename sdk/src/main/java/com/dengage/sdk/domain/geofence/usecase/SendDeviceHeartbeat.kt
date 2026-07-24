package com.dengage.sdk.domain.geofence.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.geofence.GeofenceRepository
import com.dengage.sdk.domain.geofence.model.sync.DeviceHeartbeatRequest
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SendDeviceHeartbeat : CoroutineUseCase<Response<Unit>, SendDeviceHeartbeat.Params>() {

    private val repository: GeofenceRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.sendDeviceHeartbeat(
            integrationKey = params!!.integrationKey,
            request = params.request
        )

    data class Params(
        val integrationKey: String,
        val request: DeviceHeartbeatRequest
    )
}
