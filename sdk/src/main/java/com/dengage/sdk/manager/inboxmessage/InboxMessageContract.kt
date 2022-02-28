package com.dengage.sdk.manager.inboxmessage

import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage
import com.dengage.sdk.manager.base.BasePresenter
import com.dengage.sdk.manager.base.BaseView

interface InboxMessageContract {

    interface View : BaseView {
        fun fetchedInboxMessages(inAppMessages: MutableList<InboxMessage>?)
        fun inboxMessageClicked()
        fun inboxMessageDeleted()
    }

    interface Presenter : BasePresenter<View> {
        fun getInboxMessages(
            limit: Int,
            offset: Int,
            dengageCallback: DengageCallback<MutableList<InboxMessage>>
        )

        fun setInboxMessageAsClicked(messageId: String)
        fun setInboxMessageAsDeleted(messageId: String)
        fun clearInboxMessageCache()
    }
}