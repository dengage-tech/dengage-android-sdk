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
    
    private var eventTypesMap = hashMapOf<String, EventTypeConfig>()

    private val allowedEventTypes = setOf("category_view", "product_view", "remove_from_basket", "add_to_basket")

    override fun getLayoutRes(): Int {
        return R.layout.fragment_event_history
    }

    override fun init() {
        sendPageView("event_history")
        loadEventTypesFromSdk()
        setupSpinner()
        setupRecyclerView()
        setupClickListeners()
    }

    private fun loadEventTypesFromSdk() {
        val sdkParameters = Dengage.getSdkParameters()
        eventTypesMap.clear()
        
        val systemAttributes = setOf("event_time", "device_id", "session_id", "event_type")
        
        sdkParameters?.eventMappings?.forEach { eventMapping ->
            val tableName = eventMapping.eventTableName ?: ""
            
            eventMapping.eventTypeDefinitions?.forEach { eventTypeDefinition ->
                val eventType = eventTypeDefinition.eventType
                if (!eventType.isNullOrEmpty() && tableName.isNotEmpty() && allowedEventTypes.contains(eventType)) {
                    val parameters = eventTypeDefinition.attributes?.mapNotNull { attribute ->
                        attribute.name?.let { name -> 
                            if (!systemAttributes.contains(name)) {
                                EventParameter(name, "")
                            } else {
                                null
                            }
                        }
                    } ?: emptyList()
                    
                    eventTypesMap[eventType] = EventTypeConfig(
                        tableName = tableName,
                        parameters = parameters
                    )
                }
            }
        }
        
        // Fallback to default configuration if no event mappings found
        if (eventTypesMap.isEmpty()) {
            loadDefaultEventTypes()
        }
    }
    
    private fun loadDefaultEventTypes() {
        eventTypesMap = hashMapOf(
            "category_view" to EventTypeConfig(
                tableName = "category_view_events",
                parameters = listOf(
                    EventParameter("category_id", ""),
                    EventParameter("category_name", "")
                )
            ),
            "product_view" to EventTypeConfig(
                tableName = "product_view_events",
                parameters = listOf(
                    EventParameter("product_id", ""),
                    EventParameter("product_name", ""),
                    EventParameter("price", ""),
                    EventParameter("category", "")
                )
            ),
            "remove_from_basket" to EventTypeConfig(
                tableName = "cart_events",
                parameters = listOf(
                    EventParameter("product_id", ""),
                    EventParameter("quantity", ""),
                    EventParameter("price", "")
                )
            ),
            "add_to_basket" to EventTypeConfig(
                tableName = "cart_events",
                parameters = listOf(
                    EventParameter("product_id", ""),
                    EventParameter("quantity", ""),
                    EventParameter("price", ""),
                    EventParameter("total_amount", "")
                )
            )
        )
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
                updateTableNameLabel(selectedEventType)
            }
            
            override fun onNothingSelected(parent: AdapterView<*>?) {}
        }
        
        // Set initial table name if there are event types
        if (eventTypes.isNotEmpty()) {
            updateTableNameLabel(eventTypes[0])
        }
    }

    private fun updateTableNameLabel(eventType: String) {
        val tableName = eventTypesMap[eventType]?.tableName ?: ""
        binding.tvTableName.text = getString(R.string.table_name_display, tableName)
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
        eventData["event_type"] = selectedEventType
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
