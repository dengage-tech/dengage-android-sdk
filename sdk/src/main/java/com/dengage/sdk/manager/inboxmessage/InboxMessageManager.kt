package com.dengage.sdk.manager.inboxmessage

import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage
import com.dengage.sdk.manager.base.BaseMvpManager

class InboxMessageManager : BaseMvpManager<InboxMessageContract.View, InboxMessageContract.Presenter>(),
    InboxMessageContract.View {

    override fun providePresenter() = InboxMessagePresenter()

    internal fun getInboxMessages(
        limit: Int,
        offset: Int,
        dengageCallback: DengageCallback<MutableList<InboxMessage>>
    ) {
        presenter.getInboxMessages(
            limit = limit,
            offset = offset,
            dengageCallback = dengageCallback
        )
    }

    internal fun deleteInboxMessage(
        messageId: String
    ) {
        presenter.setInboxMessageAsDeleted(
            messageId = messageId
        )
    }

    internal fun setInboxMessageAsClicked(
        messageId: String
    ) {
        presenter.setInboxMessageAsClicked(
            messageId = messageId
        )
    }

    internal fun clearInboxMessageCache() {
        presenter.clearInboxMessageCache()
    }

    override fun fetchedInboxMessages(inAppMessages: MutableList<InboxMessage>?) = Unit

    override fun inboxMessageClicked() = Unit

    override fun inboxMessageDeleted() = Unit

}