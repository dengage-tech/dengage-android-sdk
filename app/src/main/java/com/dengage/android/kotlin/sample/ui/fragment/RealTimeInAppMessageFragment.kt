package com.dengage.android.kotlin.sample.ui.fragment

import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentRealTimeInAppMessageBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class RealTimeInAppMessageFragment : BaseDataBindingFragment<FragmentRealTimeInAppMessageBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_real_time_in_app_message
    }

    override fun init() {
        sendPageView("real-time-in-app-message")

        binding.btnSetCategoryPath.setOnClickListener {
            Toast.makeText(requireContext(), "Parameter saved!", Toast.LENGTH_LONG).show()
            Dengage.setCategoryPath(binding.etCategoryPath.text.toString().trim())
        }

        binding.btnSetCartItemCount.setOnClickListener {
            Toast.makeText(requireContext(), "Parameter saved!", Toast.LENGTH_LONG).show()
            Dengage.setCartItemCount(binding.etCartItemCount.text.toString().trim())
        }

        binding.btnSetCartAmount.setOnClickListener {
            Toast.makeText(requireContext(), "Parameter saved!", Toast.LENGTH_LONG).show()
            Dengage.setCartAmount(binding.etCartAmount.text.toString().trim())
        }

        binding.btnSetState.setOnClickListener {
            Toast.makeText(requireContext(), "Parameter saved!", Toast.LENGTH_LONG).show()
            Dengage.setState(binding.etState.text.toString().trim())
        }

        binding.btnSetCity.setOnClickListener {
            Toast.makeText(requireContext(), "Parameter saved!", Toast.LENGTH_LONG).show()
            Dengage.setCity(binding.etCity.text.toString().trim())
        }

        binding.btnShowRealTimeInApp.setOnClickListener {
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

            Dengage.showRealTimeInApp(
                activity as AppCompatActivity,
                screenName.ifEmpty { null },
                customParams.ifEmpty { null }
            )
        }
    }

}