package com.dengage.android.kotlin.sample.ui.fragment

import android.widget.Toast
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentCustomEventBinding
import com.dengage.android.kotlin.sample.ui.adapter.EventParametersAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.android.kotlin.sample.ui.model.EventParameter
import com.dengage.sdk.Dengage

class CustomEventFragment : BaseDataBindingFragment<FragmentCustomEventBinding>() {

    private lateinit var adapter: EventParametersAdapter
    private val eventParameters = mutableListOf<EventParameter>()

    override fun getLayoutRes(): Int = R.layout.fragment_custom_event


    override fun init() {
        //sendPageView("custom-events")
        setupRecyclerView()
        setupClickListeners()
        setEventTableName("order_events")
        addNewParameter("event_type", "order")
        addNewParameter("page_type", "category")
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

        binding.btnSend.setOnClickListener {
            sendCustomEvent()
        }
    }

    private fun setEventTableName(tableName: String) {
        binding.etTableName.setText(tableName)
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
        val tableName = binding.etTableName.text.toString().trim()

        if (tableName.isEmpty()) {
            showToast("Please enter a table name")
            return
        }

        val eventData = HashMap<String, Any>()
        eventParameters.forEach { parameter ->
            if (parameter.key.isNotEmpty()) {
                eventData[parameter.key] = parameter.value
            }
        }

        Dengage.sendDeviceEvent(
            tableName,
            eventData
        )

        /*
        Dengage.sendCustomEvent(
            tableName,
            Dengage.getSubscription()?.contactKey!!,
            eventData
        )
        */

        //repeat(100) {

        //}


    }

    fun showToast(message: String) {
        Toast.makeText(requireContext(), message, Toast.LENGTH_SHORT).show()
    }

}