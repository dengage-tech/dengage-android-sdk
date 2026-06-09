package com.dengage.geofence.engine

import android.app.Activity
import android.content.Context
import android.location.Location
import com.dengage.geofence.manager.GeofencePermissionsHelper
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.google.firebase.messaging.RemoteMessage

/**
 * Geofence Engine v2 public API (doc 21 §6.4).
 *
 * Mevcut `com.dengage.geofence.manager` (v1) ile coexist eder; hangi engine'in aktif olduğu
 * server feature flag / `geofenceEnabled` ile yönetilir. Faz 1'de organik sync kullanılır
 * (push delivery + app lifecycle + movement); disiplinli garanti silent push Faz 2'dedir.
 */
class DengageGeofenceEngine private constructor(context: Context) {

    private val appContext = context.applicationContext
    private val engine = GeofenceEngine(appContext)

    fun start(integrationKey: String) {
        DengageLogger.verbose("DengageGeofenceEngine -> start")
        ensureInitialized()
        engine.start()
    }

    fun start() {
        DengageLogger.verbose("DengageGeofenceEngine -> start")
        ensureInitialized()
        engine.start()
    }

    fun stop() {
        DengageLogger.verbose("DengageGeofenceEngine -> stop")
        engine.stop()
    }

    /** Server'dan sync, hareket bazlı reeval, OS register, event flush (force). */
    fun forceResync() {
        engine.forceResync()
    }

    /**
     * Push delivery hook (organik sync, contract §5). Herhangi bir push teslim edildiğinde SDK
     * ETag bazlı sync check yapar. `dgs_sync_check` field'ı opsiyonel opt-out hint'idir.
     * @return sync check tetiklendiyse true.
     */
    fun handlePushDelivered(remoteMessage: RemoteMessage): Boolean {
        ensureInitialized()
        return engine.handlePushDelivered(remoteMessage.data)
    }

    fun handlePushDelivered(data: Map<String, String>?): Boolean {
        ensureInitialized()
        return engine.handlePushDelivered(data)
    }

    /** App foreground olduğunda organik sync tetikler + wake-up cap resume fırsatı. */
    fun onAppForeground() {
        engine.onAppForeground()
    }

    /** OS'tan gelen geofence transition'ı işler (receiver tarafından çağrılır). */
    internal fun handleGeofenceTransition(transitionType: Int, requestIds: List<String>, location: Location?) {
        engine.handleGeofenceTransition(transitionType, requestIds, location)
    }

    /** FLP konum güncellemesi (receiver tarafından çağrılır). */
    internal fun handleMovement(location: Location) {
        engine.handleMovement(location)
    }

    /** Wake-up cap resume girişimi (worker/push/foreground/geofence kanallarından). */
    internal fun attemptResume() {
        engine.attemptResume()
    }

    internal fun onActiveWindowBoundary() {
        engine.onActiveWindowBoundary()
    }

    fun requestLocationPermissions(activity: Activity) {
        GeofencePermissionsHelper.requestLocationPermissions(activity)
    }

    private fun ensureInitialized() {
        if (!Dengage.initialized) {
            Dengage.init(context = appContext, initForGeofence = true)
        }
    }

    companion object {
        @Volatile
        private var instance: DengageGeofenceEngine? = null

        @JvmStatic
        fun getInstance(context: Context): DengageGeofenceEngine {
            return instance ?: synchronized(this) {
                instance ?: DengageGeofenceEngine(context).also {
                    instance = it
                    ContextHolder.resetContext(context)
                }
            }
        }

        /** Server config'inde geofence açık mı (SdkParameters.geofenceEnabled). */
        @JvmStatic
        fun isEnabled(): Boolean = Prefs.sdkParameters?.geofenceEnabled ?: true
    }
}
