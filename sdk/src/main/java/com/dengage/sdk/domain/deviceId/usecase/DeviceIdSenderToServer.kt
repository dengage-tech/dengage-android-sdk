package com.dengage.sdk.domain.deviceId.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.deviceId.DeviceIdSenderRepository
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class DeviceIdSenderToServer: CoroutineUseCase<Response<Unit>, DeviceIdSenderToServer.Params>() {

    private val repository: DeviceIdSenderRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.sendDeviceIDToServer(
            token = params?.token,
            route = params?.route,
            deviceId =params?.deviceId!!


        )

    data class Params(
        val route: String,
        val token: String,
        val deviceId:String
    )
}