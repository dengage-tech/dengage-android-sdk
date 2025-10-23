package com.dengage.sdk.domain.inappmessage

import com.dengage.sdk.data.remote.api.ApiType.PUSH
import com.dengage.sdk.data.remote.api.service
import retrofit2.Response

class DebugLoggingRepository {

    private val service: DebugLoggingService by service(PUSH)

    suspend fun sendDebugLog(
        screenName: String,
        request: DebugLogRequest
    ): Response<Unit> {
        return service.sendDebugLog(screenName, request)
    }
}
