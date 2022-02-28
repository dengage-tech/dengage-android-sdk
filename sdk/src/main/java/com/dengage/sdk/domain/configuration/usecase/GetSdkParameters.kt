package com.dengage.sdk.domain.configuration.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.configuration.ConfigurationRepository
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.util.createLazy

class GetSdkParameters : CoroutineUseCase<SdkParameters, GetSdkParameters.Params>() {

    private val repository: ConfigurationRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): SdkParameters =
        repository.getSdkParameters(
            integrationKey = params!!.integrationKey
        )

    data class Params(
        val integrationKey: String
    )
}
