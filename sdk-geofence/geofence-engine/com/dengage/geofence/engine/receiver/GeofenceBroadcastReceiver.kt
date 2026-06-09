package com.dengage.geofence.engine.receiver

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.location.Location
import com.dengage.geofence.engine.DengageGeofenceEngine
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.util.DengageLogger
import com.google.android.gms.location.Geofence
import com.google.android.gms.location.GeofencingEvent
import com.google.android.gms.location.LocationResult

/**
 * Engine v2 OS event handler (doc 21 §6.6).
 * - ACTION_GEOFENCE_EVENT : `GeofencingClient` transition'ları → TriggerHandler
 * - ACTION_LOCATION_UPDATE: `FusedLocationProvider` PendingIntent güncellemeleri → MovementListener
 * - BOOT_COMPLETED        : yeniden başlatma sonrası engine'i ayağa kaldırır
 */
class GeofenceBroadcastReceiver : BroadcastReceiver() {

    override fun onReceive(context: Context, intent: Intent) {
        if (!ensureEnabled(context)) return

        when (intent.action) {
            ACTION_GEOFENCE_EVENT -> handleGeofenceEvent(context, intent)
            ACTION_LOCATION_UPDATE -> handleLocationUpdate(context, intent)
            Intent.ACTION_BOOT_COMPLETED -> handleBoot(context)
            else -> DengageLogger.debug("GeofenceBroadcastReceiver -> unhandled action ${intent.action}")
        }
    }

    private fun handleGeofenceEvent(context: Context, intent: Intent) {
        val event = GeofencingEvent.fromIntent(intent) ?: return
        if (event.hasError()) {
            DengageLogger.error("GeofenceBroadcastReceiver -> geofence error ${event.errorCode}")
            return
        }
        val transition = event.geofenceTransition
        if (transition != Geofence.GEOFENCE_TRANSITION_ENTER &&
            transition != Geofence.GEOFENCE_TRANSITION_EXIT &&
            transition != Geofence.GEOFENCE_TRANSITION_DWELL
        ) return

        val requestIds = event.triggeringGeofences?.mapNotNull { it.requestId } ?: emptyList()
        if (requestIds.isEmpty()) return

        DengageLogger.debug("GeofenceBroadcastReceiver -> transition=$transition fences=$requestIds")
        DengageGeofenceEngine.getInstance(context)
            .handleGeofenceTransition(transition, requestIds, event.triggeringLocation)
    }

    private fun handleLocationUpdate(context: Context, intent: Intent) {
        val result = LocationResult.extractResult(intent) ?: return
        val location: Location = result.lastLocation ?: return
        DengageGeofenceEngine.getInstance(context).handleMovement(location)
    }

    private fun handleBoot(context: Context) {
        DengageLogger.debug("GeofenceBroadcastReceiver -> boot completed, restarting engine")
        DengageGeofenceEngine.getInstance(context).start()
    }

    private fun ensureEnabled(context: Context): Boolean {
        if (!Dengage.initialized) {
            Dengage.init(context = context, initForGeofence = true)
        }
        val params = Prefs.sdkParameters
        if (params != null && !params.geofenceEnabled) {
            DengageLogger.debug("GeofenceBroadcastReceiver -> geofence disabled by server config")
            return false
        }
        return true
    }

    companion object {
        const val ACTION_GEOFENCE_EVENT = "com.dengage.geofence.engine.ACTION_GEOFENCE_EVENT"
        const val ACTION_LOCATION_UPDATE = "com.dengage.geofence.engine.ACTION_LOCATION_UPDATE"
    }
}
