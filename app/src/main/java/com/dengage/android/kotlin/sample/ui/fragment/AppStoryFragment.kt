package com.dengage.android.kotlin.sample.ui.fragment

import android.annotation.SuppressLint
import android.graphics.Color
import android.widget.Toast
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentAppStoryBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class AppStoryFragment : BaseDataBindingFragment<FragmentAppStoryBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_app_story
    }

    @SuppressLint("SetTextI18n")
    override fun init() {
        sendPageView("app-story")
        Dengage.setCategoryPath("23")
        binding.etPropertyId.setText("4")
        binding.etScreenName.setText("ego")
        binding.etBackgroundColor.setText("#FF0000")
        binding.btnRefreshStory.setOnClickListener {
            val storyPropertyId = binding.etPropertyId.text.toString().trim()
            val screenName = binding.etScreenName.text.toString().trim()

            val customParams = hashMapOf<String, String>()

            Dengage.showStoriesList(screenName = screenName.ifEmpty { null },
                storiesListView = binding.storiesListView,
                storyPropertyId = storyPropertyId,
                activity = requireActivity(),
                customParams = customParams.ifEmpty { null }
            )
        }

        binding.btnBackgroundColor.setOnClickListener {
            val backgroundColorText = binding.etBackgroundColor.text.toString().trim()
            try {
                val color = Color.parseColor(backgroundColorText)
                binding.storiesListView.setBackgroundColor(color)
            } catch (e: IllegalArgumentException) {
                Toast.makeText(requireContext(), "Invalid Color Code", Toast.LENGTH_SHORT).show()
            }
        }
    }

}