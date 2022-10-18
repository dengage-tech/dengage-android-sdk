package com.dengage.sdk.domain.deviceId.usecase


import com.dengage.sdk.domain.deviceId.DeviceIdModel
import retrofit2.Response
import retrofit2.http.*

interface DeviceIdSenderService {

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @POST
    suspend fun sendDeviceId(
        @Url fullUrl: String,
        @Header("Authorization") token: String?,
        @Body deviceIdObject: DeviceIdModel,
    ): Response<Unit>
}