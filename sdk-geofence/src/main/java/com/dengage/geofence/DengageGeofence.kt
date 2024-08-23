package com.dengage.geofence

import android.content.Context
import android.location.Location
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.geofence.model.GeofenceLocationSource
import com.dengage.geofence.manager.GeofenceLocationManager

object DengageGeofence {

    private val geofenceManager by lazy { GeofenceLocationManager() }

    fun handleLocation(
        context: Context,
        location: Location,
        source: GeofenceLocationSource,
        geofenceRequestId: String?,
    ) {
        if (!Dengage.initialized) {
            Dengage.init(context)
            Dengage.init(context = context, geofenceEnabled = Prefs.geofenceEnabled)
        }
        geofenceManager.handleLocation(location, source, geofenceRequestId)
    }

    fun handleBootCompleted(context: Context) {
        if (!Dengage.initialized) {
            Dengage.init(context = context, geofenceEnabled = Prefs.geofenceEnabled)
        }
        geofenceManager.handleBootCompleted()
    }

    fun startGeofence() {
        geofenceManager.startTracking()
    }

    fun stopGeofence() {
        geofenceManager.stopGeofence()
    }
}