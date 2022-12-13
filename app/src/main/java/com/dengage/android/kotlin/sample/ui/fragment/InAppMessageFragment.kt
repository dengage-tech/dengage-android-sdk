package com.dengage.android.kotlin.sample.ui.fragment

import androidx.appcompat.app.AppCompatActivity
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentInAppMessageBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class InAppMessageFragment : BaseDataBindingFragment<FragmentInAppMessageBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_in_app_message
    }

    override fun init() {
        sendPageView("in-app-message")

        binding.btnSetNavigation.setOnClickListener {
            val screenName = binding.etScreenName.text.toString().trim()
            Dengage.setNavigation(
                activity as AppCompatActivity,
                if (screenName.isEmpty()) null else screenName,123
            )
        }
    }

}