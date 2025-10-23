package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.inappmessage.InAppMessageRepository
import com.dengage.sdk.domain.inappmessage.model.InAppRemovalId
import com.dengage.sdk.util.createLazy

class GetInAppExpiredMessageIds : CoroutineUseCase<MutableList<InAppRemovalId>?, GetInAppExpiredMessageIds.Params>() {

    private val repository: InAppMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): MutableList<InAppRemovalId>? =
        repository.getInAppExpiredMessageIds(
            account = params!!.account,
            subscription = params.subscription,
            sdkParameters = params.sdkParameters

        )

    data class Params(
        val account: String,
        val subscription: Subscription,
        val sdkParameters: SdkParameters
    )
}