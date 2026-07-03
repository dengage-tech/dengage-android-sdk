package com.dengage.sdk.push

import android.content.Context
import com.dengage.sdk.util.DengageLogger

/**
 * Silent push → Geofence Engine köprüsü.
 *
 * `sdk` modülü `sdk-geofence` modülüne compile-time bağımlı olamadığı için (bağımlılık ters yönde),
 * `DengageGeofenceEngine.handleSilentPush(...)` reflection ile çağrılır. Geofence modülü classpath'te
 * yoksa (geofence kullanmayan entegrasyonlar) sessizce no-op olur.
 */
object GeofenceSilentPushDispatcher {

    private const val ENGINE_CLASS = "com.dengage.geofence.engine.DengageGeofenceEngine"
    private const val SOURCE_TYPE_KEY = "sourceType"
    private const val SOURCE_TYPE_GEOFENCE = "geofence"

    /** sourceType == geofence olan silent push'u geofence engine'e iletir. */
    fun dispatch(context: Context, data: Map<String, String?>): Boolean {
        val sourceType = data[SOURCE_TYPE_KEY]
        if (!SOURCE_TYPE_GEOFENCE.equals(sourceType, ignoreCase = true)) return false

        return try {
            val clazz = Class.forName(ENGINE_CLASS)
            val engine = clazz
                .getMethod("getInstance", Context::class.java)
                .invoke(null, context.applicationContext)
            clazz.getMethod("handleSilentPush", Map::class.java)
                .invoke(engine, data)
            DengageLogger.debug("GeofenceSilentPushDispatcher -> dispatched geofence silent push")
            true
        } catch (e: ClassNotFoundException) {
            DengageLogger.debug("GeofenceSilentPushDispatcher -> geofence engine not on classpath, ignoring")
            false
        } catch (e: Throwable) {
            DengageLogger.error("GeofenceSilentPushDispatcher -> dispatch failed: ${e.message}")
            false
        }
    }
}
