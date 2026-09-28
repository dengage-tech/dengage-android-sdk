package com.dengage.sdk.manager.event

import com.dengage.sdk.domain.event.model.PushEventType
import com.dengage.sdk.domain.event.usecase.SendEvent
import com.dengage.sdk.domain.event.usecase.SendOpenEvent
import com.dengage.sdk.domain.event.usecase.SendTransactionalOpenEvent
import com.dengage.sdk.manager.base.BaseAbstractPresenter

class EventPresenter : BaseAbstractPresenter<EventContract.View>(),
    EventContract.Presenter {

    private val sendEvent by lazy { SendEvent() }
    private val sendTransactionalOpenEvent by lazy { SendTransactionalOpenEvent() }
    private val sendOpenEvent by lazy { SendOpenEvent() }
    // Tracked per event type and message: the same event for the same message is never sent twice
    // at once, while other messages (e.g. several pushes cleared with "Clear all") are not dropped.
    private val openEventsBeingSent = mutableSetOf<Pair<PushEventType, String?>>()
    private val transactionalOpenEventsBeingSent = mutableSetOf<Pair<PushEventType, String?>>()

    override fun sendEvent(
        accountId: Int?,
        integrationKey: String,
        key: String?,
        eventTableName: String,
        eventDetails: MutableMap<String, Any>
    ) {
        sendEvent(this) {
            onResponse = {
                view { eventSent(eventTableName, key, eventDetails) }
            }
            params = SendEvent.Params(
                accountId = accountId,
                integrationKey = integrationKey,
                key = key,
                eventTableName = eventTableName,
                eventDetails = eventDetails
            )
        }
    }

    override fun sendTransactionalOpenEvent(
        buttonId: String?,
        itemId: String?,
        messageId: Int?,
        messageDetails: String?,
        transactionId: String?,
        integrationKey: String?,
        eventType: PushEventType
    ) {

        val inFlightKey = eventType to messageDetails
        if (!transactionalOpenEventsBeingSent.add(inFlightKey)) return
        sendTransactionalOpenEvent(this) {
            onResponse = {
                transactionalOpenEventsBeingSent.remove(inFlightKey)
                view { transactionalOpenEventSent() }
            }
            onError={ transactionalOpenEventsBeingSent.remove(inFlightKey)
            }
            params = SendTransactionalOpenEvent.Params(
                buttonId = buttonId,
                itemId = itemId,
                messageId = messageId,
                messageDetails = messageDetails,
                transactionId = transactionId,
                integrationKey = integrationKey,
                eventType = eventType
            )
        }
    }

    override fun sendOpenEvent(
        buttonId: String?,
        itemId: String?,
        messageId: Int?,
        messageDetails: String?,
        integrationKey: String?,
        eventType: PushEventType
    ) {
        val inFlightKey = eventType to messageDetails
        if (!openEventsBeingSent.add(inFlightKey)) return

        sendOpenEvent(this) {
            onResponse = {
                openEventsBeingSent.remove(inFlightKey)
                view { openEventSent() }
            }
            onError ={
                openEventsBeingSent.remove(inFlightKey)

            }
            params = SendOpenEvent.Params(
                buttonId = buttonId,
                itemId = itemId,
                messageId = messageId,
                messageDetails = messageDetails,
                integrationKey = integrationKey,
                eventType = eventType
            )
        }
    }

}
