package com.dengage.android.kotlin.sample.ui.fragment

import android.app.Activity
import androidx.appcompat.app.AppCompatActivity
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentGeofenceBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class GeofenceFragment : BaseDataBindingFragment<FragmentGeofenceBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_geofence
    }

    override fun init() {
        sendPageView("geofence")

        binding.btnRequestLocationPermission.setOnClickListener {
            val activity = this.activity as AppCompatActivity
            Dengage.requestLocationPermissions(activity)
        }

        binding.btnStopGeofencing.setOnClickListener {
            Dengage.stopGeofence()
        }

    }
}