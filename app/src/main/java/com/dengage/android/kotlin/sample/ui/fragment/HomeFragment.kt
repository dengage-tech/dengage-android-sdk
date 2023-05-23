package com.dengage.android.kotlin.sample.ui.fragment

import androidx.navigation.fragment.findNavController
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentHomeBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage
import com.dengage.sdk.manager.inappmessage.util.InAppMessageUtils
import org.joda.time.DateTime
import java.util.Calendar

class HomeFragment : BaseDataBindingFragment<FragmentHomeBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_home
    }

    override fun init() {
        sendPageView("home")

        binding.btnDeviceInfo.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToDeviceInfo())
            Dengage.setDevelopmentStatus(true)


        }

        binding.btnUserPermission.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToUserPermission())
        }

        binding.btnContactKey.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToContactKey())
        }

        binding.btnCountry.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToCountry())
           // Dengage.sendDeviceIdToServer("V1/dengage/sync/mobile/customerData","jp8c615tk235gjfd378r9bwjlkzhq6m7")
        }

        binding.btnInboxMessages.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToInboxMessages())
        }

        binding.btnCustomEvents.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToCustomEvent())
        }

        binding.btnInAppMessage.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToInAppMessage())
        }

        binding.btnTags.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToTags())
        }

        binding.btnDengageTestPage.setOnClickListener {
            Dengage.showTestPage(requireActivity())
        }
    }

}