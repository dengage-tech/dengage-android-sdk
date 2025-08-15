package com.dengage.sdk.manager.event

import com.dengage.sdk.manager.base.BasePresenter
import com.dengage.sdk.manager.base.BaseView

interface EventContract {

    interface View : BaseView {
        fun eventSent(tableName: String, key: String?, eventDetails: Map<String, Any>)
        fun transactionalOpenEventSent()
        fun openEventSent()
    }

    interface Presenter : BasePresenter<View> {
        fun sendEvent(
            accountId: Int?,
            integrationKey: String,
            key: String?,
            eventTableName: String,
            eventDetails: MutableMap<String, Any>
        )

        fun sendTransactionalOpenEvent(
            buttonId: String?,
            itemId: String?,
            messageId: Int?,
            messageDetails: String?,
            transactionId: String?,
            integrationKey: String?
        )

        fun sendOpenEvent(
            buttonId: String?,
            itemId: String?,
            messageId: Int?,
            messageDetails: String?,
            integrationKey: String?
        )
    }
}