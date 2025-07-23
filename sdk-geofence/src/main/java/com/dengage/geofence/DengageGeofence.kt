package com.dengage.geofence

import android.app.Activity
import android.content.Context
import android.location.Location
import android.util.Log
import com.dengage.geofence.manager.GeofenceLocationManager
import com.dengage.geofence.manager.GeofencePermissionsHelper
import com.dengage.sdk.Dengage
import com.dengage.sdk.domain.geofence.model.GeofenceLocationSource
import com.dengage.sdk.util.DengageLogger

object DengageGeofence {

    private val geofenceManager by lazy { GeofenceLocationManager() }

    fun handleLocation(
        context: Context,
        location: Location,
        source: GeofenceLocationSource,
        geofenceRequestId: String?,
    ) {
        if (!Dengage.initialized) {
            Dengage.init(context = context, initForGeofence = true)
        }
        geofenceManager.handleLocation(location, source, geofenceRequestId)
    }

    fun handleBootCompleted(context: Context) {
        if (!Dengage.initialized) {
            Dengage.init(context = context, initForGeofence = true)
        }
        geofenceManager.handleBootCompleted()
    }

    fun startGeofence() {
        DengageLogger.verbose("DengageGeofence -> startTracking")

        geofenceManager.startTracking()
    }

    fun stopGeofence() {
        DengageLogger.verbose("DengageGeofence -> stopTracking")
        geofenceManager.stopGeofence()
    }

    fun requestLocationPermissions(activity: Activity) {
        GeofencePermissionsHelper.requestLocationPermissions(activity)
    }
}