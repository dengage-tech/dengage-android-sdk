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
    private var isOpenEventBeingSent :Boolean = false
    private var isTransactionalOpenEventBeingSent :Boolean = false

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
        integrationKey: String?
    ) {

        if(isTransactionalOpenEventBeingSent) return
        isTransactionalOpenEventBeingSent=true
        sendTransactionalOpenEvent(this) {
            onResponse = {
                view { isTransactionalOpenEventBeingSent=false
                    transactionalOpenEventSent() }
            }
            onError={ isTransactionalOpenEventBeingSent=false
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
        if(isOpenEventBeingSent) return
        isOpenEventBeingSent=true

        sendOpenEvent(this) {
            onResponse = {
                view {
                    isOpenEventBeingSent=false
                    openEventSent() }
            }
            onError ={
                isOpenEventBeingSent=false

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
