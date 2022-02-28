package com.dengage.android.kotlin.sample.ui.fragment

import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentCountryBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class CountryFragment : BaseDataBindingFragment<FragmentCountryBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_country
    }

    override fun init() {
        sendPageView("country")

        binding.etCountry.setText(Dengage.getSubscription()?.country)

        binding.btnSave.setOnClickListener {
            val country = binding.etCountry.text.toString().trim()
            if (country.isNotEmpty()) {
                Dengage.setCountry(country)
            }
        }
    }
}