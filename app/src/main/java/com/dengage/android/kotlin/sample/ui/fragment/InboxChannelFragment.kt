package com.dengage.android.kotlin.sample.ui.fragment

import android.view.View
import android.widget.Toast
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentInboxChannelBinding
import com.dengage.android.kotlin.sample.ui.adapter.InboxChannelAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage
import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.callback.DengageError
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEvent
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEventType
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelMessage

class InboxChannelFragment : BaseDataBindingFragment<FragmentInboxChannelBinding>(),
    InboxChannelAdapter.InboxChannelCallback,
    DengageCallback<MutableList<InboxChannelMessage>> {

    private val messages = mutableListOf<InboxChannelMessage>()
    private val adapter = InboxChannelAdapter(messages, this)

    override fun getLayoutRes(): Int = R.layout.fragment_inbox_channel

    override fun init() {
        binding.rvInboxChannel.adapter = adapter
        fetchMessages()
    }

    private fun fetchMessages() {
        Dengage.getInboxChannelMessages(limit = 20, dengageCallback = this)
    }

    override fun onResult(result: MutableList<InboxChannelMessage>) {
        binding.rvInboxChannel.post {
            messages.clear()
            messages.addAll(result)
            adapter.setItems(messages)
            updateEmptyState()
            // Report an impression (IM) for every message currently shown, in bulk.
            sendImpressions(result)
        }
    }

    override fun onError(error: DengageError) {
        Toast.makeText(context, error.errorMessage, Toast.LENGTH_LONG).show()
    }

    private fun sendImpressions(shown: List<InboxChannelMessage>) {
        if (shown.isEmpty()) return
        val events = shown.map { it.toEvent(InboxChannelEventType.IMPRESSION) }
        Dengage.sendInboxChannelEvents(events)
    }

    override fun onOpen(message: InboxChannelMessage) {
        Dengage.sendInboxChannelEvents(listOf(message.toEvent(InboxChannelEventType.OPEN)))
        message.isRead = true
        adapter.setItems(messages)
    }

    override fun onClick(message: InboxChannelMessage) {
        Dengage.sendInboxChannelEvents(listOf(message.toEvent(InboxChannelEventType.CLICK)))
        Toast.makeText(context, "CTA clicked (CL sent)", Toast.LENGTH_SHORT).show()
    }

    override fun onDelete(message: InboxChannelMessage) {
        // Reflect the delete on the UI immediately; the server processes the event async.
        Dengage.sendInboxChannelEvents(listOf(message.toEvent(InboxChannelEventType.DELETE)))
        messages.removeAll { it.id == message.id }
        adapter.setItems(messages)
        updateEmptyState()
    }

    private fun InboxChannelMessage.toEvent(type: InboxChannelEventType) = InboxChannelEvent(
        eventType = type,
        messageId = id,
        messageDetails = data.messageDetails
    )

    private fun updateEmptyState() {
        binding.tvEmpty.visibility = if (messages.isEmpty()) View.VISIBLE else View.GONE
    }
}
