package com.dengage.android.kotlin.sample.ui.fragment

import android.view.View
import android.widget.AdapterView
import android.widget.ArrayAdapter
import android.widget.Toast
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentEventHistoryBinding
import com.dengage.android.kotlin.sample.ui.adapter.EventParametersAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.android.kotlin.sample.ui.model.EventParameter
import com.dengage.sdk.Dengage

class EventHistoryFragment : BaseDataBindingFragment<FragmentEventHistoryBinding>() {

    private lateinit var adapter: EventParametersAdapter
    private val eventParameters = mutableListOf<EventParameter>()
    
    data class EventTypeConfig(
        val tableName: String,
        val parameters: List<EventParameter>
    )
    
    private val eventTypesMap = hashMapOf(
        "order" to EventTypeConfig(
            tableName = "order_events",
            parameters = listOf(
                EventParameter("page_type", ""),
                EventParameter("category", ""),
                EventParameter("user_id", "")
            )
        ),
        "Product View" to EventTypeConfig(
            tableName = "product_view_events",
            parameters = listOf(
                EventParameter("product_id", ""),
                EventParameter("product_name", ""),
                EventParameter("price", ""),
                EventParameter("category", "")
            )
        ),
        "Add to Cart" to EventTypeConfig(
            tableName = "cart_events",
            parameters = listOf(
                EventParameter("product_id", ""),
                EventParameter("quantity", ""),
                EventParameter("price", ""),
                EventParameter("total_amount", "")
            )
        ),
        "Purchase" to EventTypeConfig(
            tableName = "purchase_events",
            parameters = listOf(
                EventParameter("order_id", ""),
                EventParameter("total_amount", ""),
                EventParameter("currency", ""),
                EventParameter("items_count", "")
            )
        ),
        "User Registration" to EventTypeConfig(
            tableName = "registration_events",
            parameters = listOf(
                EventParameter("user_id", ""),
                EventParameter("email", ""),
                EventParameter("registration_method", "")
            )
        )
    )

    override fun getLayoutRes(): Int {
        return R.layout.fragment_event_history
    }

    override fun init() {
        sendPageView("event_history")
        setupSpinner()
        setupRecyclerView()
        setupClickListeners()
    }

    private fun setupSpinner() {
        val eventTypes = eventTypesMap.keys.toList()
        val spinnerAdapter = ArrayAdapter(requireContext(), android.R.layout.simple_spinner_item, eventTypes)
        spinnerAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item)
        binding.spinnerEventType.adapter = spinnerAdapter
        
        binding.spinnerEventType.onItemSelectedListener = object : AdapterView.OnItemSelectedListener {
            override fun onItemSelected(parent: AdapterView<*>?, view: View?, position: Int, id: Long) {
                val selectedEventType = eventTypes[position]
                loadParametersForEventType(selectedEventType)
            }
            
            override fun onNothingSelected(parent: AdapterView<*>?) {}
        }
    }

    private fun setupRecyclerView() {
        adapter = EventParametersAdapter(eventParameters) { position ->
            removeParameter(position)
        }
        binding.rvEventParameters.adapter = adapter
    }

    private fun setupClickListeners() {
        binding.btnAddParameter.setOnClickListener {
            addNewParameter()
        }

        binding.btnSendEvent.setOnClickListener {
            sendCustomEvent()
        }
    }

    private fun loadParametersForEventType(eventType: String) {
        eventParameters.clear()
        eventTypesMap[eventType]?.let { config ->
            eventParameters.addAll(config.parameters.map { EventParameter(it.key, it.value) })
        }
        adapter.setItems(eventParameters)
    }

    private fun addNewParameter(key: String = "", value: String = "") {
        val newParameter = EventParameter(key, value)
        eventParameters.add(newParameter)
        adapter.addItem(newParameter, eventParameters.size - 1)
    }

    private fun removeParameter(position: Int) {
        if (eventParameters.size > 1) {
            eventParameters.removeAt(position)
            adapter.removeItem(position)
        }
    }

    private fun sendCustomEvent() {
        val selectedEventType = binding.spinnerEventType.selectedItem.toString()

        if (selectedEventType.isEmpty()) {
            showToast("Please select an event type")
            return
        }

        val eventConfig = eventTypesMap[selectedEventType]
        if (eventConfig == null) {
            showToast("Invalid event type")
            return
        }

        val eventData = HashMap<String, Any>()
        eventParameters.forEach { parameter ->
            if (parameter.key.isNotEmpty()) {
                eventData[parameter.key] = parameter.value
            }
        }

        Dengage.sendDeviceEvent(eventConfig.tableName, eventData)
        showToast("Event sent to table: ${eventConfig.tableName}")
    }

    private fun showToast(message: String) {
        Toast.makeText(requireContext(), message, Toast.LENGTH_SHORT).show()
    }
}
