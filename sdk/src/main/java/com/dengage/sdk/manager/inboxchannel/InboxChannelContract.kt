package com.dengage.sdk.manager.inboxchannel

import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEvent
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelMessage
import com.dengage.sdk.manager.base.BasePresenter
import com.dengage.sdk.manager.base.BaseView

interface InboxChannelContract {

    interface View : BaseView {
        fun fetchedInboxChannelMessages(messages: MutableList<InboxChannelMessage>?)
        fun inboxChannelEventsSent()
    }

    interface Presenter : BasePresenter<View> {
        fun getInboxChannelMessages(
            limit: Int,
            dengageCallback: DengageCallback<MutableList<InboxChannelMessage>>
        )

        fun sendInboxChannelEvents(events: List<InboxChannelEvent>)
    }
}
