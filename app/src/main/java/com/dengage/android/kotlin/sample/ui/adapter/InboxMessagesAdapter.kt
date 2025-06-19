package com.dengage.android.kotlin.sample.ui.adapter

import android.view.ViewGroup
import android.widget.TextView
import androidx.appcompat.app.AlertDialog
import androidx.appcompat.widget.AppCompatButton
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.ui.base.BaseRecyclerViewAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseRecyclerViewHolder
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage
import java.text.SimpleDateFormat
import java.util.*

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

            val markAsReadButton = itemView.findViewById<AppCompatButton>(R.id.btn_mark_as_read)

            if (item.isClicked) {
                markAsReadButton.visibility = android.view.View.GONE
            } else {
                markAsReadButton.visibility = android.view.View.VISIBLE
                markAsReadButton.setOnClickListener {
                    inboxMessageCallback.markAsRead(item.id)
                }
            }

            itemView.findViewById<AppCompatButton>(R.id.btn_delete).setOnClickListener {
                AlertDialog.Builder(itemView.context)
                    .setTitle("Delete Message")
                    .setMessage("Are you sure you want to delete this message?")
                    .setPositiveButton("Delete") { _, _ ->
                        inboxMessageCallback.delete(item.id)
                    }
                    .setNegativeButton("Cancel", null)
                    .show()
            }

            itemView.findViewById<TextView>(R.id.tv_title).text = item.data.title
            itemView.findViewById<TextView>(R.id.tv_message).text = item.data.message

            val formattedDate = item.data.receiveDate?.let { formatDate(it) }
            itemView.findViewById<TextView>(R.id.tv_receive_date).text =
                itemView.context.getString(R.string.receive_date_time, formattedDate)

            itemView.findViewById<TextView>(R.id.tv_is_clicked).text =
                itemView.context.getString(R.string.read_status, item.isClicked.toString())

        }

        private fun formatDate(dateString: String): String {
            return try {
                val inputFormat = SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSSSSS'Z'", Locale.getDefault())
                inputFormat.timeZone = TimeZone.getTimeZone("UTC")
                val date = inputFormat.parse(dateString)

                val outputFormat = SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault())
                outputFormat.format(date ?: Date())
            } catch (e: Exception) {
                dateString
            }
        }
    }

    interface InboxMessageCallback {
        fun markAsRead(id: String)
        fun delete(id: String)
    }
}