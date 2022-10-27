package com.dengage.sdk.domain.configuration

import com.dengage.sdk.data.remote.api.ApiType
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.configuration.model.VisitorInfo

class ConfigurationRepository {

    private val service: ConfigurationService by service(ApiType.PUSH)

    suspend fun getSdkParameters(
        integrationKey: String
    ): SdkParameters {
        return service.getSdkParameters(
            integrationKey = integrationKey
        )
    }

    suspend fun getVisitorInfo(
        accountName: String?,
        contactKey: String?,
        deviceId: String,
    ): VisitorInfo {
        return service.getVisitorInfo(
            accountName = accountName,
            contactKey = contactKey,
            deviceId = deviceId
        )
    }
}
