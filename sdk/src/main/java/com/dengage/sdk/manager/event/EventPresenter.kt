package com.dengage.sdk.manager.event

import com.dengage.sdk.domain.event.usecase.SendEvent
import com.dengage.sdk.domain.event.usecase.SendOpenEvent
import com.dengage.sdk.domain.event.usecase.SendTransactionalOpenEvent
import com.dengage.sdk.manager.base.BaseAbstractPresenter

class EventPresenter : BaseAbstractPresenter<EventContract.View>(),
    EventContract.Presenter {

    private val sendEvent by lazy { SendEvent() }
    private val sendTransactionalOpenEvent by lazy { SendTransactionalOpenEvent() }
    private val sendOpenEvent by lazy { SendOpenEvent() }

    override fun sendEvent(
        accountId: Int?,
        integrationKey: String,
        key: String?,
        eventTableName: String,
        eventDetails: MutableMap<String, Any>
    ) {
        sendEvent(this) {
            onResponse = {
                view { eventSent() }
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
        integrationKey: String?
    ) {
        sendTransactionalOpenEvent(this) {
            onResponse = {
                view { transactionalOpenEventSent() }
            }
            params = SendTransactionalOpenEvent.Params(
                buttonId = buttonId,
                itemId = itemId,
                messageId = messageId,
                messageDetails = messageDetails,
                transactionId = transactionId,
                integrationKey = integrationKey
            )
        }
    }

    override fun sendOpenEvent(
        buttonId: String?,
        itemId: String?,
        messageId: Int?,
        messageDetails: String?,
        integrationKey: String?
    ) {
        sendOpenEvent(this) {
            onResponse = {
                view { openEventSent() }
            }
            params = SendOpenEvent.Params(
                buttonId = buttonId,
                itemId = itemId,
                messageId = messageId,
                messageDetails = messageDetails,
                integrationKey = integrationKey
            )
        }
    }

}
