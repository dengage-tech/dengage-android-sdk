package com.dengage.android.kotlin.sample.ui.adapter

import android.view.View
import android.view.ViewGroup
import android.widget.AdapterView
import android.widget.ArrayAdapter
import android.widget.Spinner
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
        private val spinnerEventValue = itemView.findViewById<Spinner>(R.id.spinnerEventValue)
        private val btnRemove = itemView.findViewById<AppCompatButton>(R.id.btnRemove)

        override fun bindItem(item: EventParameter) {
            etEventKey.setText(item.key)
            etEventKey.isEnabled = !item.isReadOnly
            
            when (item.inputType) {
                EventParameter.InputType.TEXT -> {
                    etEventValue.visibility = View.VISIBLE
                    spinnerEventValue.visibility = View.GONE
                    
                    etEventValue.setText(item.value)
                    etEventValue.isEnabled = !item.isReadOnly
                    
                    etEventValue.addTextChangedListener {
                        item.value = it.toString()
                    }
                }
                EventParameter.InputType.DROPDOWN -> {
                    etEventValue.visibility = View.GONE
                    spinnerEventValue.visibility = View.VISIBLE
                    
                    val adapter = ArrayAdapter(itemView.context, android.R.layout.simple_spinner_item, item.options)
                    adapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item)
                    spinnerEventValue.adapter = adapter
                    
                    val selectedIndex = item.options.indexOf(item.value).takeIf { it >= 0 } ?: 0
                    spinnerEventValue.setSelection(selectedIndex)
                    
                    spinnerEventValue.onItemSelectedListener = object : AdapterView.OnItemSelectedListener {
                        override fun onItemSelected(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
                            item.value = item.options[position]
                        }
                        override fun onNothingSelected(parent: AdapterView<*>?) {}
                    }
                }
            }

            etEventKey.addTextChangedListener {
                item.key = it.toString()
            }

            btnRemove.setOnClickListener {
                onRemoveItem(adapterPosition)
            }
            
            btnRemove.isEnabled = !item.isReadOnly
        }
    }
}