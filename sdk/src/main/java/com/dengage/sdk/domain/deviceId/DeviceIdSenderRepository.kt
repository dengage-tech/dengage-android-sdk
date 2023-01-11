package com.dengage.sdk.domain.deviceId

import com.dengage.sdk.data.remote.api.ApiType
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.deviceId.model.DeviceIdModel
import com.dengage.sdk.domain.deviceId.usecase.DeviceIdSenderService
import retrofit2.Response

class DeviceIdSenderRepository    {
    private val service: DeviceIdSenderService by service(ApiType.PUSH)

suspend fun sendDeviceIDToServer(
    route: String,
    token: String,
    deviceId:String
): Response<Unit> {
    return service.sendDeviceId(route, "Bearer $token", deviceIdObject = DeviceIdModel(deviceId))
}
}