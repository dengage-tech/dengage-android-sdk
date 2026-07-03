package com.dengage.android.kotlin.sample.ui.fragment

import androidx.appcompat.app.AppCompatActivity
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentGeofenceBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.geofence.DengageGeofence
import com.dengage.geofence.engine.DengageGeofenceEngine
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale

class GeofenceFragment : BaseDataBindingFragment<FragmentGeofenceBinding>() {

    private val dateFormat = SimpleDateFormat("dd.MM.yyyy HH:mm:ss", Locale.getDefault())

    override fun getLayoutRes(): Int {
        return R.layout.fragment_geofence
    }

    override fun init() {
        sendPageView("geofence")

        binding.btnRequestLocationPermission.setOnClickListener {
            val activity = this.activity as AppCompatActivity
            DengageGeofence.requestLocationPermissions(activity)
        }

        binding.btnStopGeofencing.setOnClickListener {
            DengageGeofence.stopGeofence()
        }

        binding.btnLastSilentPushSync.setOnClickListener {
            showLastSilentPushSync()
        }

        // Açılışta da mevcut değeri göster
        showLastSilentPushSync()
    }

    private fun showLastSilentPushSync() {
        val lastSync = DengageGeofenceEngine.getInstance(requireContext()).lastSilentPushSyncAt()
        binding.txtLastSilentPushSync.text = if (lastSync == null || lastSync <= 0L) {
            getString(R.string.label_last_silent_push_sync_none)
        } else {
            getString(R.string.label_last_silent_push_sync, dateFormat.format(Date(lastSync)))
        }
    }
}
