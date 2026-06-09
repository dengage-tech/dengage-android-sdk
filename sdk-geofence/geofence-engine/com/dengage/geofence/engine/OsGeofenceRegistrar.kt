package com.dengage.geofence.engine

import android.annotation.SuppressLint
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.os.Build
import com.dengage.geofence.engine.receiver.GeofenceBroadcastReceiver
import com.dengage.geofence.engine.storage.model.Fence
import com.dengage.sdk.domain.geofence.model.sync.GeofenceTriggerType
import com.dengage.sdk.util.DengageLogger
import com.google.android.gms.location.Geofence
import com.google.android.gms.location.GeofencingClient
import com.google.android.gms.location.GeofencingRequest
import com.google.android.gms.location.LocationServices

/**
 * OS geofence register/unregister (doc 21 §6.5, K9).
 * `setInitialTrigger(GEOFENCE_TRANSITION_ENTER)` her register'da set edilir — uzun mesafe yer
 * değişimi (uçuş) sonrası cihaz fence içindeyse anında enter event tetiklenir (havalimanı senaryosu).
 */
@SuppressLint("MissingPermission")
class OsGeofenceRegistrar(private val context: Context) {

    private val client: GeofencingClient = LocationServices.getGeofencingClient(context)

    fun register(fences: List<Fence>) {
        removeAll {
            if (fences.isEmpty()) {
                DengageLogger.debug("OsGeofenceRegistrar -> nothing to register")
                return@removeAll
            }
            val osGeofences = fences.mapNotNull { buildGeofence(it) }
            if (osGeofences.isEmpty()) return@removeAll

            val request = GeofencingRequest.Builder()
                .setInitialTrigger(GeofencingRequest.INITIAL_TRIGGER_ENTER)
                .addGeofences(osGeofences)
                .build()

            client.addGeofences(request, pendingIntent())
                .addOnSuccessListener {
                    DengageLogger.debug("OsGeofenceRegistrar -> registered ${osGeofences.size} fences")
                }
                .addOnFailureListener {
                    DengageLogger.error("OsGeofenceRegistrar -> register failed: ${it.message}")
                }
        }
    }

    fun removeAll(onComplete: () -> Unit = {}) {
        client.removeGeofences(pendingIntent())
            .addOnCompleteListener { onComplete() }
    }

    private fun buildGeofence(fence: Fence): Geofence? {
        return try {
            var transitions = Geofence.GEOFENCE_TRANSITION_ENTER or Geofence.GEOFENCE_TRANSITION_EXIT
            val dwellMinutes = fence.campaigns
                .filter { it.triggerType == GeofenceTriggerType.DWELL }
                .mapNotNull { it.dwellMinutes }
                .maxOrNull()

            val builder = Geofence.Builder()
                .setRequestId(fence.requestId)
                .setCircularRegion(fence.latitude, fence.longitude, fence.radiusM.toFloat())
                .setExpirationDuration(Geofence.NEVER_EXPIRE)

            if (dwellMinutes != null && dwellMinutes > 0) {
                transitions = transitions or Geofence.GEOFENCE_TRANSITION_DWELL
                builder.setLoiteringDelay(dwellMinutes * 60_000)
            }

            builder.setTransitionTypes(transitions).build()
        } catch (e: Exception) {
            DengageLogger.error("OsGeofenceRegistrar -> build failed for ${fence.requestId}: ${e.message}")
            null
        }
    }

    private fun pendingIntent(): PendingIntent {
        val intent = Intent(context, GeofenceBroadcastReceiver::class.java)
            .setAction(GeofenceBroadcastReceiver.ACTION_GEOFENCE_EVENT)
        var flags = PendingIntent.FLAG_UPDATE_CURRENT
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            flags = flags or PendingIntent.FLAG_MUTABLE
        }
        return PendingIntent.getBroadcast(context, REQUEST_CODE, intent, flags)
    }

    companion object {
        private const val REQUEST_CODE = 0xD6F2
    }
}
