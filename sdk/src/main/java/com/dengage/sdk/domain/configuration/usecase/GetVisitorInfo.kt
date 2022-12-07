package com.dengage.sdk.domain.configuration.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.configuration.ConfigurationRepository
import com.dengage.sdk.domain.configuration.model.VisitorInfo
import com.dengage.sdk.util.createLazy

class GetVisitorInfo : CoroutineUseCase<VisitorInfo, GetVisitorInfo.Params>() {

    private val repository: ConfigurationRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): VisitorInfo =
        repository.getVisitorInfo(
            accountName = params!!.accountName,
            contactKey = params.contactKey,
            deviceId = params.deviceId
        )

    data class Params(
        val accountName: String?,
        val contactKey: String?,
        val deviceId: String
    )
}
