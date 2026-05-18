package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inappmessage.InAppMessageRepository
import com.dengage.sdk.domain.inappmessage.model.AbTestAssignment

class AssignAbTestVariant : CoroutineUseCase<AbTestAssignment?, AssignAbTestVariant.Params>() {

    private val repository by lazy { InAppMessageRepository() }

    override suspend fun buildUseCase(params: Params?): AbTestAssignment? {
        return repository.assignAbTestVariant(
            accountId = params!!.accountId,
            appId = params.appId,
            campaignId = params.campaignId
        )
    }

    data class Params(
        val accountId: String,
        val appId: String?,
        val campaignId: String
    )
}
