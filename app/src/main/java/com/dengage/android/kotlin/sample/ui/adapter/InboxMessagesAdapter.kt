package com.dengage.android.kotlin.sample.ui.adapter

import android.view.ViewGroup
import android.widget.TextView
import androidx.appcompat.widget.AppCompatButton
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.ui.base.BaseRecyclerViewAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseRecyclerViewHolder
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage

class InboxMessagesAdapter(
    items: List<InboxMessage>? = arrayListOf(),
    var inboxMessageCallback: InboxMessageCallback
) : BaseRecyclerViewAdapter(items) {
    override fun createNewViewHolder(parent: ViewGroup, viewType: Int) =
        ViewHolder(parent)

    inner class ViewHolder(
        parent: ViewGroup
    ) : BaseRecyclerViewHolder<InboxMessage>(parent, R.layout.recycler_item_inbox_message) {

        override fun bindItem(item: InboxMessage) {

            itemView.findViewById<AppCompatButton>(R.id.btn_mark_as_read).setOnClickListener {
                inboxMessageCallback.markAsRead(item.id)
            }
            itemView.findViewById<AppCompatButton>(R.id.btn_delete).setOnClickListener {
                inboxMessageCallback.delete(item.id)
            }

            itemView.findViewById<TextView>(R.id.tv_title).text = item.data.title
            itemView.findViewById<TextView>(R.id.tv_message).text = item.data.message
            itemView.findViewById<TextView>(R.id.tv_receive_date).text = item.data.receiveDate
            itemView.findViewById<TextView>(R.id.tv_is_clicked).text = item.isClicked.toString()

        }
    }

    interface InboxMessageCallback {
        fun markAsRead(id: String)
        fun delete(id: String)
    }
}