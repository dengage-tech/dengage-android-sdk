package com.dengage.sdk.domain.event.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.event.EventRepository
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SendEvent : CoroutineUseCase<Response<Unit>, SendEvent.Params>() {

    private val repository: EventRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.sendEvent(
             accountId=params!!.accountId,
            integrationKey = params.integrationKey,
            key = params.key,
            eventTableName = params.eventTableName,
            eventDetails = params.eventDetails,
        )

    data class Params(
        val accountId:Int?,
        val integrationKey: String,
        val key: String?,
        val eventTableName: String,
        val eventDetails: MutableMap<String, Any>
    )
}
