package com.dengage.sdk.domain.event

import com.dengage.sdk.data.remote.api.ApiType
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.event.model.Event
import com.dengage.sdk.domain.event.model.OpenEvent
import com.dengage.sdk.domain.event.model.TransactionalOpenEvent
import retrofit2.Response

class EventRepository {

    private val eventService: EventService by service(ApiType.EVENT)
    private val openEventService: OpenEventService by service(ApiType.PUSH)

    suspend fun sendEvent(
        accountId: Int?,
        integrationKey: String,
        key: String?,
        eventTableName: String,
        eventDetails: MutableMap<String, Any>
    ): Response<Unit> {
        return eventService.sendEvent(
            event = Event(
                accountId = accountId,
                integrationKey = integrationKey,
                key = key,
                eventTableName = eventTableName,
                eventDetails = eventDetails
            )
        )
    }

    suspend fun sendTransactionalOpenEvent(
        buttonId: String?,
        itemId: String?,
        messageId: Int?,
        messageDetails: String?,
        transactionId: String?,
        integrationKey: String?
    ): Response<Unit> {
        return openEventService.sendTransactionalOpenEvent(
            transactionalOpenEvent = TransactionalOpenEvent(
                buttonId = buttonId,
                itemId = itemId,
                messageId = messageId,
                messageDetails = messageDetails,
                transactionId = transactionId,
                integrationKey = integrationKey
            )
        )
    }

    suspend fun sendOpenEvent(
        buttonId: String?,
        itemId: String?,
        messageId: Int?,
        messageDetails: String?,
        integrationKey: String?
    ): Response<Unit> {
        return openEventService.sendOpenEvent(
            openEvent = OpenEvent(
                buttonId = buttonId,
                itemId = itemId,
                messageId = messageId,
                messageDetails = messageDetails,
                integrationKey = integrationKey
            )
        )
    }
}
