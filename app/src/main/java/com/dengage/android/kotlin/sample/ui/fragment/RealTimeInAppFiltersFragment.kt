package com.dengage.android.kotlin.sample.ui.fragment

import androidx.navigation.fragment.findNavController
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentRealTimeInAppFiltersBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment

class RealTimeInAppFiltersFragment : BaseDataBindingFragment<FragmentRealTimeInAppFiltersBinding>() {

    override fun getLayoutRes(): Int =  R.layout.fragment_real_time_in_app_filters

    override fun init() {
        sendPageView("real_time_in_app_filters")

        binding.btnEventHistory.setOnClickListener {
            findNavController().navigate(RealTimeInAppFiltersFragmentDirections.actionRealTimeInAppFiltersToEventHistory())
        }

        binding.btnCart.setOnClickListener {
            findNavController().navigate(RealTimeInAppFiltersFragmentDirections.actionRealTimeInAppFiltersToCart())
        }
    }
}
