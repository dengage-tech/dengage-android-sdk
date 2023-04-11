package com.dengage.sdk.domain.deviceId.usecase

import com.dengage.sdk.SenderDeviceIdResponse
import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.deviceId.DeviceIdSenderRepository
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class DeviceIdSenderToServer: CoroutineUseCase<List<SenderDeviceIdResponse>, DeviceIdSenderToServer.Params>() {

    private val repository: DeviceIdSenderRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): List<SenderDeviceIdResponse> =
        repository.sendDeviceIDToServer(
            token = params!!.token,
            route = params.route,
            deviceId = params.deviceId


        )

    data class Params(
        val route: String,
        val token: String,
        val deviceId:String
    )
}