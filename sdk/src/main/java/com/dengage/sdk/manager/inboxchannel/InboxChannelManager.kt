package com.dengage.sdk.manager.inboxchannel

import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEvent
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelMessage
import com.dengage.sdk.manager.base.BaseMvpManager

class InboxChannelManager :
    BaseMvpManager<InboxChannelContract.View, InboxChannelContract.Presenter>(),
    InboxChannelContract.View {

    override fun providePresenter() = InboxChannelPresenter()

    internal fun getInboxChannelMessages(
        limit: Int,
        dengageCallback: DengageCallback<MutableList<InboxChannelMessage>>
    ) {
        presenter.getInboxChannelMessages(
            limit = limit,
            dengageCallback = dengageCallback
        )
    }

    internal fun sendInboxChannelEvents(events: List<InboxChannelEvent>) {
        presenter.sendInboxChannelEvents(events)
    }

    override fun fetchedInboxChannelMessages(messages: MutableList<InboxChannelMessage>?) = Unit

    override fun inboxChannelEventsSent() = Unit
}
