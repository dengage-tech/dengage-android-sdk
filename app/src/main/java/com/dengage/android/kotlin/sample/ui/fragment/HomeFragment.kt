package com.dengage.android.kotlin.sample.ui.fragment

import android.util.Log
import android.widget.Toast
import androidx.navigation.fragment.findNavController
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentHomeBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage
import com.dengage.sdk.callback.ReviewDialogCallback

class HomeFragment : BaseDataBindingFragment<FragmentHomeBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_home
    }

    override fun init() {
        sendPageView("home")

        binding.btnDeviceInfo.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToDeviceInfo())
            Dengage.setDevelopmentStatus(true)

            // Dengage.setDevelopmentStatus(false)
            /*     activity?.let { it1 ->
                     Dengage.showRatingDialog(activity = it1,
                         reviewDialogCallback = object : ReviewDialogCallback {
                             override fun onCompletion() {
                                 // this@InAppMessageActivity.finish()
                                 Log.d("oops", "complete")
                             }

                             override fun onError() {

                                 Log.d("oops", "error")
                             }

                         })
                 }*/
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
            //findNavController().navigate(HomeFragmentDirections.actionHomeToCustomEvent())
            showRating2()
        }

        binding.btnInAppMessage.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToInAppMessage())
        }

        binding.btnTags.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToTags())
        }

        binding.btnDengageTestPage.setOnClickListener {
              Dengage.showTestPage(requireActivity())
           // showRating()
        }
    }
    fun showRating() {
        Dengage.showRatingDialog(activity = requireActivity(),
            reviewDialogCallback = object : ReviewDialogCallback {
                override fun onCompletion() {
                    //requireActivity().finish()
                    Toast.makeText(requireActivity(),"complete 1", Toast.LENGTH_LONG).show()
                }

                override fun onError() {
                   // requireActivity().finish()
                    Toast.makeText(requireActivity(),"error 1", Toast.LENGTH_LONG).show()

                }

            })
         requireActivity().finish()
    }

    fun showRating2() {
        Dengage.showRatingDialog(activity = requireActivity(),
            reviewDialogCallback = object : ReviewDialogCallback {
                override fun onCompletion() {
                     requireActivity().finish()
                    Toast.makeText(requireActivity(),"complete", Toast.LENGTH_LONG).show()
                }

                override fun onError() {
                    requireActivity().finish()
                    Toast.makeText(requireActivity(),"error", Toast.LENGTH_LONG).show()

                }

            })
        //requireActivity().finish()
    }
}