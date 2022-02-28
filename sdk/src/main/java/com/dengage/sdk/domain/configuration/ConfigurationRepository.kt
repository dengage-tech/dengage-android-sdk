package com.dengage.sdk.domain.configuration

import com.dengage.sdk.data.remote.api.ApiType
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.configuration.model.SdkParameters

class ConfigurationRepository {

    private val service: ConfigurationService by service(ApiType.PUSH)

    suspend fun getSdkParameters(
        integrationKey: String
    ): SdkParameters {
        return service.getSdkParameters(
            integrationKey = integrationKey
        )
    }
}
