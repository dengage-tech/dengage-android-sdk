package com.dengage.sdk.domain.event.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.event.EventRepository
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SendOpenEvent : CoroutineUseCase<Response<Unit>, SendOpenEvent.Params>() {

    private val repository: EventRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.sendOpenEvent(
            buttonId = params!!.buttonId,
            itemId = params.itemId,
            messageId = params.messageId,
            messageDetails = params.messageDetails,
            integrationKey = params.integrationKey,
        )

    data class Params(
        val buttonId: String?,
        val itemId: String?,
        val messageId: Int?,
        val messageDetails: String?,
        val integrationKey: String?
    )
}
