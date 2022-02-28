package com.dengage.android.kotlin.sample.ui.fragment

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

        binding.tvIntegrationKey.text = getString(
            R.string.dengage_integration_key,
            Dengage.getSubscription()?.integrationKey
        )
        binding.tvDeviceId.text = getString(
            R.string.dengage_device_id,
            Dengage.getSubscription()?.deviceId
        )
        binding.tvAdvertisingId.text = getString(
            R.string.dengage_advertising_id,
            Dengage.getSubscription()?.advertisingId
        )
        binding.tvToken.text = getString(
            R.string.dengage_token,
            Dengage.getToken()
        )
        binding.tvContactKey.text = getString(
            R.string.dengage_contact_key,
            Dengage.getSubscription()?.contactKey
        )
        binding.tvTimeZone.text = getString(
            R.string.dengage_timezone,
            Dengage.getSubscription()?.timezone
        )
        binding.tvLanguage.text = getString(
            R.string.dengage_language,
            Dengage.getSubscription()?.language
        )
        binding.tvUserPermission.text = getString(
            R.string.dengage_user_permission,
            Dengage.getUserPermission().toString()
        )
    }

}