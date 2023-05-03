package com.dengage.android.kotlin.sample.ui.fragment

import android.content.Intent
import androidx.navigation.fragment.findNavController
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentHomeBinding
import com.dengage.android.kotlin.sample.ui.activity.MainActivity2
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage
import com.dengage.sdk.util.extension.launchActivity

class HomeFragment : BaseDataBindingFragment<FragmentHomeBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_home
    }

    override fun init() {
        sendPageView("home")

        binding.btnDeviceInfo.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToDeviceInfo())


        }

        binding.btnUserPermission.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToUserPermission())
        }

        binding.btnContactKey.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToContactKey())
        }

        binding.btnOpenTC.setOnClickListener {
            startActivity(Intent(activity,MainActivity2::class.java))
            activity?.finishAffinity()
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

        binding.btnRealTimeInAppMessage.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToRealTimeInAppMessage())
        }

        binding.btnTags.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToTags())
        }

        binding.btnDengageTestPage.setOnClickListener {
            Dengage.showTestPage(requireActivity())
        }


    }

}