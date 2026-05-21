package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inappmessage.InAppMessageRepository
import com.dengage.sdk.domain.inappmessage.model.AbTestAssignmentResponse
import com.dengage.sdk.util.createLazy

class AssignAbTestVariant : CoroutineUseCase<AbTestAssignmentResponse?, AssignAbTestVariant.Params>() {

    private val repository: InAppMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): AbTestAssignmentResponse? =
        repository.assignAbTestVariant(
            accountId = params!!.accountId,
            appId = params.appId,
            campaignId = params.campaignId
        )

    data class Params(
        val accountId: String,
        val appId: String,
        val campaignId: String
    )
}
