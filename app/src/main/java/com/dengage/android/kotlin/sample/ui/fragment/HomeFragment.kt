package com.dengage.android.kotlin.sample.ui.fragment

import android.content.Intent
import androidx.navigation.fragment.findNavController
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentHomeBinding
import com.dengage.android.kotlin.sample.ui.activity.MainActivity2
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class HomeFragment : BaseDataBindingFragment<FragmentHomeBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_home
    }

    override fun init() {
        sendPageView("home")

        binding.btnDeviceInfo.setOnClickListener {
           findNavController().navigate(HomeFragmentDirections.actionHomeToDeviceInfo())
           Dengage.setDevelopmentStatus(isDebug = true)
        //Dengage.getLastPushPayload()
//Dengage.setDeviceId("sdss")
            //startActivity(Intent(activity, MainActivity2::class.java))

        }

        binding.btnUserPermission.setOnClickListener {
        //  findNavController().navigate(HomeFragmentDirections.actionHomeToUserPermission())
            activity?.let { it1 -> Dengage.setNavigation(it1,"hasnain123") }
        }

        binding.btnContactKey.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToContactKey())

        }

        binding.btnCountry.setOnClickListener {
          //  findNavController().navigate(HomeFragmentDirections.actionHomeToCountry())
            activity?.let { it1 -> Dengage.setNavigation(it1,"hasnain") }

        }

        binding.btnInboxMessages.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToInboxMessages())
                // Dengage.removeInAppMessageDisplay()
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

        binding.btnInAppinline.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToinappInline())
        }
    }

}