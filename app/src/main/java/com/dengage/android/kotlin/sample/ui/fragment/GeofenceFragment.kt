package com.dengage.android.kotlin.sample.ui.fragment

import androidx.appcompat.app.AppCompatActivity
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentGeofenceBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.geofence.DengageGeofence
import com.dengage.geofenceengine.DengageGeofenceEngine
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import kotlin.math.roundToInt

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

        binding.btnMonitoredGeofences.setOnClickListener {
            showMonitoredGeofences()
        }

        binding.btnTriggeredEvents.setOnClickListener {
            showTriggeredEvents()
        }

        // Açılışta da mevcut değeri göster
        showLastSilentPushSync()
    }

    private fun showMonitoredGeofences() {
        val fences = DengageGeofenceEngine.getInstance(requireContext()).monitoredGeofences()
        if (fences.isEmpty()) {
            binding.txtGeofenceDiagnostics.text = getString(R.string.label_monitored_geofences_none)
            return
        }
        binding.txtGeofenceDiagnostics.text = buildString {
            append("Monitored Geofences (${fences.size})\n\n")
            fences.forEach { fence ->
                append("#${fence.geofenceId} ${fence.title ?: "(no title)"}\n")
                append("  cluster: ${fence.clusterId} · state: ${fence.state}\n")
                append(
                    "  %.5f, %.5f · r=%dm\n\n".format(
                        fence.latitude, fence.longitude, fence.radiusM.toInt()
                    )
                )
            }
        }
    }

    private fun showTriggeredEvents() {
        val events = DengageGeofenceEngine.getInstance(requireContext()).recentTriggeredEvents()
        if (events.isEmpty()) {
            binding.txtGeofenceDiagnostics.text = getString(R.string.label_triggered_events_none)
            return
        }
        binding.txtGeofenceDiagnostics.text = buildString {
            append("Triggered Events (${events.size})\n\n")
            events.forEach { event ->
                val campaigns = if (event.campaignIds.isEmpty()) {
                    "no matching campaign"
                } else {
                    "campaigns: ${event.campaignIds.joinToString(", ")}"
                }
                val accuracy = event.accuracyM?.let { "accuracy: ${it.roundToInt()}m" } ?: "accuracy: n/a"
                val campaignsLine = if (event.stateOnly) "state-only (no push) · $campaigns" else campaigns
                append("${event.eventType.uppercase()} · #${event.geofenceId} ${event.title ?: "(no title)"}\n")
                append("  ${dateFormat.format(Date(event.occurredAtMillis))}\n")
                append("  $accuracy\n")
                append("  $campaignsLine\n\n")
            }
        }
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
