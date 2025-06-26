package com.dengage.android.kotlin.sample.ui.fragment

import android.view.View
import android.widget.Toast
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.appcompat.app.AlertDialog
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
        fetchInboxMessages(0)
        updateToolbarVisibility()
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

        binding.toolbar.setOnMenuItemClickListener { menuItem ->
            when (menuItem.itemId) {
                R.id.action_mark_all_read -> {
                    AlertDialog.Builder(requireContext())
                        .setTitle("Mark All Read")
                        .setMessage("Are you sure you want to mark all messages as read?")
                        .setPositiveButton("Mark All Read") { _, _ ->
                            Dengage.setAllInboxMessagesAsClicked()
                            inboxMessages.forEach { it.isClicked = true }
                            adapter.setItems(inboxMessages)
                            AlertDialog.Builder(requireContext())
                                .setTitle("Read All Messages")
                                .setMessage("All messages have been marked as read.")
                                .setPositiveButton("OK", null)
                                .show()
                        }
                        .setNegativeButton("Cancel", null)
                        .show()
                    true
                }
                R.id.action_delete_all -> {
                    AlertDialog.Builder(requireContext())
                        .setTitle("Delete All Messages")
                        .setMessage("Are you sure you want to delete all messages?")
                        .setPositiveButton("Delete All") { _, _ ->
                            Dengage.deleteAllInboxMessages()
                            inboxMessages.clear()
                            adapter.setItems(inboxMessages)
                            AlertDialog.Builder(requireContext())
                                .setTitle("Deleted All Messages")
                                .setMessage("All messages have been successfully deleted.")
                                .setPositiveButton("OK", null)
                                .show()
                            updateToolbarVisibility()
                        }
                        .setNegativeButton("Cancel", null)
                        .show()
                    true
                }
                else -> false
            }
        }
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
        updateToolbarVisibility()
    }

    override fun delete(id: String) {
        val messageToDelete = inboxMessages.firstOrNull { it.id == id }
        Dengage.deleteInboxMessage(id)
        inboxMessages.removeAll { inboxMessage -> inboxMessage.id == id }
        adapter.setItems(inboxMessages)
        updateToolbarVisibility()
        AlertDialog.Builder(requireContext())
            .setTitle("Deleted")
            .setMessage(
                if (messageToDelete != null) {
                    "Message \"${messageToDelete.data.title}\" has been successfully deleted."
                } else {
                    "Message has been successfully deleted."
                }
            )
            .setPositiveButton("OK", null)
            .show()
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
        binding.rvInboxMessages.post {
            inboxMessages.addAll(result)
            adapter.addItems(result)
            updateToolbarVisibility()
        }
    }

    private fun updateToolbarVisibility() {
        binding.toolbar.visibility =
            if (inboxMessages.isEmpty()) View.GONE
            else View.VISIBLE
    }


}