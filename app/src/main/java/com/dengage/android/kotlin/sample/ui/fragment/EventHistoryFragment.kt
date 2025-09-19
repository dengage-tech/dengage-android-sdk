package com.dengage.android.kotlin.sample.ui.fragment

import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentEventHistoryBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class EventHistoryFragment : BaseDataBindingFragment<FragmentEventHistoryBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_event_history
    }

    override fun init() {
        sendPageView("event_history")

        binding.btnSendEvent.setOnClickListener {
            val eventName = binding.etEventName.text.toString()
            val eventParams = hashMapOf<String, Any>()
            
            if (eventName.isNotEmpty()) {
                //Dengage.sendCustomEvent(eventName, eventParams)
                //showToast("Event sent: $eventName")
            } else {
                //showToast("Please enter event name")
            }
        }
    }
}
