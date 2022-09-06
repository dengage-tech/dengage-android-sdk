package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inappmessage.InAppMessageRepository
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.createLazy

class GetRealTimeInAppMessages : CoroutineUseCase<MutableList<InAppMessage>?, GetRealTimeInAppMessages.Params>() {

    private val repository: InAppMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): MutableList<InAppMessage>? =
        repository.getRealTimeInAppMessages(
            accountId = params!!.accountId,
            appId = params.appId
        )?.map {
            InAppMessage(
                id = it.publicId ?: DengageUtils.generateUUID(),
                data = it
            )
        }?.toMutableList()

    data class Params(
        val accountId: String,
        val appId: String
    )
}
