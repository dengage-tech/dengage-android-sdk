package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inappmessage.GetRealTimeInAppRepository
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.createLazy

class GetRealTimeInAppMessagesV2 : CoroutineUseCase<MutableList<InAppMessage>?, GetRealTimeInAppMessagesV2.Params>() {

    private val repository: GetRealTimeInAppRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): MutableList<InAppMessage>? =
        repository.getRealTimeInAppMessagesV2(
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
