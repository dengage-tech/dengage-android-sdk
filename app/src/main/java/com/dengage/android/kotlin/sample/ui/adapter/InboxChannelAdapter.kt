package com.dengage.android.kotlin.sample.ui.adapter

import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.appcompat.app.AlertDialog
import androidx.appcompat.widget.AppCompatButton
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.ui.base.BaseRecyclerViewAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseRecyclerViewHolder
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelMessage
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import java.util.TimeZone

class InboxChannelAdapter(
    items: List<InboxChannelMessage>? = arrayListOf(),
    var callback: InboxChannelCallback
) : BaseRecyclerViewAdapter(items) {

    override fun createNewViewHolder(parent: ViewGroup, viewType: Int) = ViewHolder(parent)

    inner class ViewHolder(
        parent: ViewGroup
    ) : BaseRecyclerViewHolder<InboxChannelMessage>(parent, R.layout.recycler_item_inbox_channel) {

        override fun bindItem(item: InboxChannelMessage) {
            itemView.findViewById<TextView>(R.id.tv_title).text = item.data.title
            itemView.findViewById<TextView>(R.id.tv_message).text = item.data.message

            itemView.findViewById<TextView>(R.id.tv_meta).text =
                itemView.context.getString(
                    R.string.inbox_channel_meta,
                    item.priority,
                    item.data.isPinned.toString(),
                    item.isRead.toString()
                )

            val formattedDate = item.data.receiveDate?.let { formatDate(it) }
            itemView.findViewById<TextView>(R.id.tv_receive_date).text =
                itemView.context.getString(R.string.receive_date_time, formattedDate)

            val openButton = itemView.findViewById<AppCompatButton>(R.id.btn_open)
            if (item.isRead) {
                openButton.visibility = View.GONE
            } else {
                openButton.visibility = View.VISIBLE
                openButton.setOnClickListener { callback.onOpen(item) }
            }

            val ctaButton = itemView.findViewById<AppCompatButton>(R.id.btn_cta)
            val firstCta = item.data.ctaButtons?.firstOrNull()
            if (firstCta == null) {
                ctaButton.visibility = View.GONE
            } else {
                ctaButton.visibility = View.VISIBLE
                ctaButton.text = firstCta.label ?: itemView.context.getString(R.string.inbox_channel_cta)
                ctaButton.setOnClickListener { callback.onClick(item) }
            }

            itemView.findViewById<AppCompatButton>(R.id.btn_delete).setOnClickListener {
                AlertDialog.Builder(itemView.context)
                    .setTitle("Delete Message")
                    .setMessage("Are you sure you want to delete this message?")
                    .setPositiveButton("Delete") { _, _ -> callback.onDelete(item) }
                    .setNegativeButton("Cancel", null)
                    .show()
            }
        }

        private fun formatDate(dateString: String): String {
            return try {
                val inputFormat =
                    SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'", Locale.getDefault())
                inputFormat.timeZone = TimeZone.getTimeZone("UTC")
                val date = inputFormat.parse(dateString)
                val outputFormat = SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault())
                outputFormat.format(date ?: Date())
            } catch (e: Exception) {
                dateString
            }
        }
    }

    interface InboxChannelCallback {
        fun onOpen(message: InboxChannelMessage)
        fun onClick(message: InboxChannelMessage)
        fun onDelete(message: InboxChannelMessage)
    }
}
