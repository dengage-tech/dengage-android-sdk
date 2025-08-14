package com.dengage.android.kotlin.sample.ui.fragment

import android.widget.Toast
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentCustomEventBinding
import com.dengage.android.kotlin.sample.ui.adapter.EventParametersAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.android.kotlin.sample.ui.model.EventParameter
import com.dengage.sdk.Dengage

class CustomEventFragment : BaseDataBindingFragment<FragmentCustomEventBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_custom_event
    }

    val mobileEventTest = "mobile_event_test"

    override fun init() {
        sendPageView("custom-events")

        val eventParameters = mutableListOf<EventParameter>()
        for (i in 0 until 2) {
            eventParameters.add(EventParameter("param${i + 1}", null))
        }
        binding.rvEventParameters.adapter = EventParametersAdapter(eventParameters)

        binding.etTableName.setText(mobileEventTest)

        binding.btnSend.setOnClickListener {
            val customEventData = hashMapOf<String, Any>()

            for (eventParameter in eventParameters) {
                if( eventParameter.key.isNullOrEmpty()) {
                    showToast("Please enter a key for all parameters")
                    return@setOnClickListener
                }
                if (eventParameter.value == null) {
                    showToast("Please enter a value for all parameters")
                    return@setOnClickListener
                }
                customEventData[eventParameter.key!!] = eventParameter.value ?: ""
            }
            val cartItems= arrayOf (HashMap<String, Any>())


            val item1: HashMap<String, Any> = HashMap()
            item1["product_id"] = "1234"
            item1["product_variant_id"] = "12224"
            item1["quantity"] = 1
            item1["unit_price"] = 9.99
            item1["discounted_price"] = 9.99
// ... extra columns in shopping_cart_events_detail table, can be added here
// ... extra columns in shopping_cart_events_detail table, can be added here
            cartItems[0]=item1


            customEventData.put("cartItems",cartItems)
            Dengage.order(
                customEventData
            )
        }
    }

    fun showToast(message: String) {
        Toast.makeText(requireContext(), message, Toast.LENGTH_SHORT).show()
    }

}