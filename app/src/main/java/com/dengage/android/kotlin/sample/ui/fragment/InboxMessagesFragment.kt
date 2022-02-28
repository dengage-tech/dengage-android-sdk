package com.dengage.android.kotlin.sample.ui.fragment

import android.widget.Toast
import androidx.recyclerview.widget.LinearLayoutManager
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentInboxMessagesBinding
import com.dengage.android.kotlin.sample.ui.adapter.InboxMessagesAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.android.kotlin.sample.ui.base.RecyclerViewPaginationListener
import com.dengage.sdk.Dengage
import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.callback.DengageError
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage

class InboxMessagesFragment : BaseDataBindingFragment<FragmentInboxMessagesBinding>(),
    InboxMessagesAdapter.InboxMessageCallback,
    DengageCallback<MutableList<InboxMessage>> {

    private var isLoading = false
    private var inboxMessages = mutableListOf<InboxMessage>()
    private val adapter = InboxMessagesAdapter(inboxMessages, this)

    override fun getLayoutRes(): Int {
        return R.layout.fragment_inbox_messages
    }

    override fun init() {
        sendPageView("inbox-messages")

        fetchInboxMessages(0)
        binding.rvInboxMessages.adapter = adapter

        binding.rvInboxMessages.addOnScrollListener(object : RecyclerViewPaginationListener(
            binding.rvInboxMessages.layoutManager as LinearLayoutManager, 20, 17
        ) {
            override fun loadMore(currentPage: Int) {
                fetchInboxMessages(adapter.itemCount)
            }

            override fun isLoading(): Boolean {
                return isLoading
            }
        })
    }

    private fun fetchInboxMessages(offset: Int) {
        isLoading = true
        Dengage.getInboxMessages(20, offset, this)
    }

    override fun markAsRead(id: String) {
        Dengage.setInboxMessageAsClicked(id)
        val inboxMessageToMarkAsRead =
            inboxMessages.firstOrNull { inboxMessage -> inboxMessage.id == id }
        inboxMessageToMarkAsRead?.isClicked = true
        adapter.setItems(inboxMessages)
    }

    override fun delete(id: String) {
        Dengage.deleteInboxMessage(id)
        inboxMessages.removeAll { inboxMessage -> inboxMessage.id == id }
        adapter.setItems(inboxMessages)
    }

    override fun onError(error: DengageError) {
        isLoading = false
        Toast.makeText(
            context, error.errorMessage,
            Toast.LENGTH_LONG
        ).show()
    }

    override fun onResult(result: MutableList<InboxMessage>) {
        isLoading = false
        inboxMessages.addAll(result)
        adapter.addItems(result)
    }

}