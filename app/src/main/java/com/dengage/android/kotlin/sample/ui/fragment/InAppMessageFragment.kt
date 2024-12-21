package com.dengage.android.kotlin.sample.ui.fragment

import android.widget.Toast
import androidx.appcompat.app.AppCompatActivity
import androidx.recyclerview.widget.LinearLayoutManager
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentInAppMessageBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.android.kotlin.sample.ui.adapter.DeviceInfoAdapter
import com.dengage.sdk.Dengage

class InAppMessageFragment : BaseDataBindingFragment<FragmentInAppMessageBinding>() {

    private val deviceInfoAdapter = DeviceInfoAdapter()

    override fun getLayoutRes(): Int {
        return R.layout.fragment_in_app_message
    }

    override fun init() {
        sendPageView("in-app-message")

        setupRecyclerView()
        loadInitialDeviceInfo()

        binding.btnSetNavigation.setOnClickListener {
            val screenName = binding.etScreenName.text.toString().trim()
            saveDeviceInfo()
            Dengage.setNavigation(
                activity as AppCompatActivity,
                screenName
            )
        }

        binding.btnClearDeviceInfo.setOnClickListener {
            Dengage.clearInAppDeviceInfo()
            deviceInfoAdapter.clearData()
            addNewDeviceInfoRow()
        }

        binding.btnAddDeviceInfo.setOnClickListener {
            addNewDeviceInfoRow()
        }
    }

    private fun setupRecyclerView() {
        binding.rvDeviceInfo.layoutManager = LinearLayoutManager(context)
        binding.rvDeviceInfo.adapter = deviceInfoAdapter
    }

    private fun loadInitialDeviceInfo() {
        val deviceInfo = Dengage.getInAppDeviceInfo()
        if (deviceInfo.isNotEmpty()) {
            deviceInfo.forEach { (key, value) ->
                deviceInfoAdapter.addDeviceInfo(key, value)
            }
        }
        addNewDeviceInfoRow()
    }

    private fun addNewDeviceInfoRow() {
        deviceInfoAdapter.addDeviceInfo("", "")
    }

    private fun saveDeviceInfo() {
        val deviceInfo = deviceInfoAdapter.getDeviceInfoList()
        deviceInfo.forEach { (key, value) ->
            if (key.isNotEmpty() && value.isNotEmpty()) {
                Dengage.setInAppDeviceInfo(key, value)
            }
        }
    }

}