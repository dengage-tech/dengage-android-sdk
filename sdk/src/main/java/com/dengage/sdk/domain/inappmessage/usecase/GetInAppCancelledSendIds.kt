package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inappmessage.InAppMessageRepository
import com.dengage.sdk.domain.inappmessage.model.InAppCancelledSendId
import com.dengage.sdk.util.createLazy

class GetInAppCancelledSendIds : CoroutineUseCase<MutableList<InAppCancelledSendId>?, GetInAppCancelledSendIds.Params>() {

    private val repository: InAppMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): MutableList<InAppCancelledSendId>? =
        repository.getInAppCancelledSendIds(
            account = params!!.account
        )

    data class Params(
        val account: String
    )
}
