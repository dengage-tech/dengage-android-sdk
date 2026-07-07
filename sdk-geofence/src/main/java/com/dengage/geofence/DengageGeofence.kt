package com.dengage.geofence

import android.app.Activity
import android.content.Context
import android.location.Location
import com.dengage.geofenceengine.DengageGeofenceEngine
import com.dengage.geofence.manager.GeofencePermissionsHelper
import com.dengage.sdk.Dengage
import com.dengage.sdk.domain.geofence.model.GeofenceLocationSource
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger

/**
 * Public geofence facade. Artık yeni Geofence Engine (v2) kullanır; eski `GeofenceLocationManager`
 * (v1) devre dışıdır. `geofenceEnabled` + izin kontrolü engine içinde yapılır.
 */
object DengageGeofence {

    @JvmStatic
    var geofenceInterceptor: GeofenceInterceptor? = null

    private fun engine(context: Context) = DengageGeofenceEngine.getInstance(context)

    private fun appContext(): Context? = try {
        ContextHolder.context
    } catch (e: Exception) {
        DengageLogger.error("DengageGeofence -> context not available: ${e.message}")
        null
    }

    fun startGeofence() {
        DengageLogger.verbose("DengageGeofence -> startGeofence (v2)")
        val context = appContext() ?: return
        engine(context).start()
    }

    fun stopGeofence() {
        DengageLogger.verbose("DengageGeofence -> stopGeofence (v2)")
        val context = appContext() ?: return
        engine(context).stop()
    }

    fun requestLocationPermissions(activity: Activity) {
        GeofencePermissionsHelper.requestLocationPermissions(activity)
    }

    fun handleBootCompleted(context: Context) {
        if (!Dengage.initialized) {
            Dengage.init(context = context, initForGeofence = true)
        }
        DengageLogger.debug("DengageGeofence -> handleBootCompleted (v2)")
        engine(context).start()
    }

    /**
     * Geriye dönük uyum: v1 receiver/host'un çağırdığı konum girişi. v2 engine kendi
     * receiver'ı üzerinden hareketleri işler; buraya gelen konum best-effort engine'e iletilir.
     */
    fun handleLocation(
        context: Context,
        location: Location,
        source: GeofenceLocationSource,
        geofenceRequestId: String?,
    ) {
        if (!Dengage.initialized) {
            Dengage.init(context = context, initForGeofence = true)
        }
        engine(context).handleMovement(location)
    }
}
