package com.dengage.android.kotlin.sample.ui.adapter

import android.view.ViewGroup
import androidx.appcompat.widget.AppCompatButton
import androidx.appcompat.widget.AppCompatEditText
import androidx.core.widget.addTextChangedListener
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.ui.base.BaseRecyclerViewAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseRecyclerViewHolder
import com.dengage.android.kotlin.sample.ui.model.EventParameter

class EventParametersAdapter(
    items: List<EventParameter> = arrayListOf(),
    private val onRemoveItem: (position: Int) -> Unit
) : BaseRecyclerViewAdapter(items) {
    
    override fun createNewViewHolder(parent: ViewGroup, viewType: Int) =
        ViewHolder(parent)

    inner class ViewHolder(
        parent: ViewGroup
    ) : BaseRecyclerViewHolder<EventParameter>(parent, R.layout.recycler_item_custom_event) {

        private val etEventKey = itemView.findViewById<AppCompatEditText>(R.id.etEventKey)
        private val etEventValue = itemView.findViewById<AppCompatEditText>(R.id.etEventValue)
        private val btnRemove = itemView.findViewById<AppCompatButton>(R.id.btnRemove)

        override fun bindItem(item: EventParameter) {
            etEventKey.setText(item.key)
            etEventValue.setText(item.value)

            etEventKey.addTextChangedListener {
                item.key = it.toString()
            }
            
            etEventValue.addTextChangedListener {
                item.value = it.toString()
            }

            btnRemove.setOnClickListener {
                onRemoveItem(adapterPosition)
            }
        }
    }
}