package com.dengage.android.kotlin.sample.ui.fragment

import android.R.attr.label
import android.content.ClipData
import android.content.ClipboardManager
import android.content.Context
import android.content.res.Resources
import android.os.Build
import android.widget.Toast
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentDeviceInfoBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage


class DeviceInfoFragment : BaseDataBindingFragment<FragmentDeviceInfoBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_device_info
    }

    override fun init() {
        sendPageView("device-info")
        Dengage.sendLoginEvent()
        Dengage.sendLogoutEvent()
        Dengage.sendRegisterEvent()

        binding.tvIntegrationKey.text = getString(
            R.string.dengage_integration_key,
            Dengage.getSubscription()?.integrationKey
        )
        binding.tvDeviceId.text = getString(
            R.string.dengage_device_id,
            Dengage.getSubscription()?.deviceId
        )
        binding.tvContactKey.text = getString(
            R.string.dengage_contact_key,
            Dengage.getSubscription()?.contactKey
        )
        binding.tvUserPermission.text = getString(
            R.string.dengage_user_permission,
            Dengage.getUserPermission().toString()
        )
        binding.tvToken.text = getString(
            R.string.dengage_token,
            Dengage.getToken()
        )



        binding.tvDeviceId.setOnClickListener {
            val clipboard: ClipboardManager =
                activity?.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
            val clip = ClipData.newPlainText("di", binding.tvDeviceId.text)
            clipboard.setPrimaryClip(clip)
            Toast.makeText(activity,"copied",Toast.LENGTH_SHORT).show()
        }

       binding.tvToken.setOnClickListener {
            val clipboard: ClipboardManager =
                activity?.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
            val clip = ClipData.newPlainText("ik", binding.tvToken.text)
            clipboard.setPrimaryClip(clip)
           Toast.makeText(activity,"copied",Toast.LENGTH_SHORT).show()
        }



        binding.tvContactKey.setOnClickListener {
            val clipboard: ClipboardManager =
                activity?.getSystemService(Context.CLIPBOARD_SERVICE) as ClipboardManager
            val clip = ClipData.newPlainText("ck", binding.tvContactKey.text)
            clipboard.setPrimaryClip(clip)
            Toast.makeText(activity,"copied",Toast.LENGTH_SHORT).show()
        }




        binding.tvDeviceBrand.text = getString(
            R.string.dengage_device_brand,
            Build.BRAND
        )
        binding.tvDeviceModel.text = getString(
            R.string.dengage_device_model,
            Build.MODEL
        )
        binding.tvAdvertisingId.text = getString(
            R.string.dengage_advertising_id,
            Dengage.getSubscription()?.advertisingId
        )


        binding.tvTimeZone.text = getString(
            R.string.dengage_timezone,
            Dengage.getSubscription()?.timezone
        )
        binding.tvLanguage.text = getString(
            R.string.dengage_language,
            Dengage.getSubscription()?.language
        )

        binding.tvSdkVersion.text = getString(
            R.string.dengage_sdk_version,
            Dengage.getSdkVersion()
        )

        binding.tvScreenWidth.text = getString(
            R.string.dengage_screen_width,
            Resources.getSystem().displayMetrics.widthPixels.toString()
        )
        binding.tvScreenHeight.text = getString(
            R.string.dengage_screen_height,
            Resources.getSystem().displayMetrics.heightPixels.toString()
        )
    }

}