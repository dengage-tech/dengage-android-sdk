package com.dengage.android.kotlin.sample.ui.fragment

import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentInappInlineBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class InAppInLineFragment : BaseDataBindingFragment<FragmentInappInlineBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_inapp_inline
    }

    override fun init() {
        sendPageView("in-app-message")

        binding.btnShowInlineInapp.setOnClickListener {
            var propertyId = binding.etPropertyId.text.toString().trim()
            val screenName = binding.etScreenName.text.toString().trim()
            val customKey1 = binding.etCustomKey1.text.toString().trim()
            val customValue1 = binding.etCustomValue1.text.toString().trim()
            val customKey2 = binding.etCustomKey2.text.toString().trim()
            val customValue2 = binding.etCustomValue2.text.toString().trim()

            val customParams = hashMapOf<String, String>()
            if (customKey1.isNotEmpty() && customValue1.isNotEmpty()) {
                customParams[customKey1] = customValue1
            }
            if (customKey2.isNotEmpty() && customValue2.isNotEmpty()) {
                customParams[customKey2] = customValue2
            }
            Dengage.showInlineInApp(screenName = screenName.ifEmpty { null },
                inAppInlineElement = binding.webview,
                propertyId = propertyId,
                activity = requireActivity(),
                customParams = customParams.ifEmpty { null }
            )
        }
    }

}