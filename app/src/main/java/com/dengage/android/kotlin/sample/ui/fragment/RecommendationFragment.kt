package com.dengage.android.kotlin.sample.ui.fragment

import android.graphics.Color
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentRecommendationBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class RecommendationFragment : BaseDataBindingFragment<FragmentRecommendationBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_recommendation
    }

    override fun init() {
        binding.etPropertyId.setText("1")
        binding.etScreenName.setText("recommendation")
        binding.btnShowRecommendation.setOnClickListener {
            val propertyId = binding.etPropertyId.text.toString().trim()
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
            //binding.recommendationView.setBackgroundColor(Color.LTGRAY)
            Dengage.getRecommendation(
                recommendationPropertyId = propertyId,
                recommendationView = binding.recommendationView,
                activity = requireActivity(),
                screenName = screenName.ifEmpty { null },
                customParams = customParams.ifEmpty { null }
            )
        }
    }
}
