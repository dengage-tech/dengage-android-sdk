package com.dengage.sdk.domain.event.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.event.EventRepository
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SendTransactionalOpenEvent : CoroutineUseCase<Response<Unit>, SendTransactionalOpenEvent.Params>() {

    private val repository: EventRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.sendTransactionalOpenEvent(
            buttonId = params!!.buttonId,
            itemId = params.itemId,
            messageId = params.messageId,
            messageDetails = params.messageDetails,
            transactionId = params.transactionId,
            integrationKey = params.integrationKey,
        )

    data class Params(
        val buttonId: String?,
        val itemId: String?,
        val messageId: Int?,
        val messageDetails: String?,
        val transactionId: String?,
        val integrationKey: String?
    )
}
