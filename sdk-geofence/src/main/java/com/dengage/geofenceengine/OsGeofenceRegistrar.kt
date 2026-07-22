package com.dengage.geofenceengine

import android.annotation.SuppressLint
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.os.Build
import com.dengage.geofenceengine.receiver.GeofenceBroadcastReceiver
import com.dengage.geofenceengine.storage.model.Fence
import com.dengage.geofence.manager.GeofenceLocationReceiver
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
    private val registeredIdsStore = RegisteredFenceIdsStore(context)

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
                    // GeofencingClient kayıtlı geofence'leri sorgulama API'si sunmuyor
                    // (iOS'taki `monitoredRegions` muadili yok) → teşhis için kendimiz tutuyoruz.
                    registeredIdsStore.save(osGeofences.map { it.requestId })
                }
                .addOnFailureListener {
                    DengageLogger.error("OsGeofenceRegistrar -> register failed: ${it.message}")
                    registeredIdsStore.save(emptyList())
                    GeofenceDebugLogger.error(
                        "Geofence OS register failed",
                        mapOf("error" to (it.message ?: "unknown"), "count" to osGeofences.size.toString())
                    )
                }
        }
    }

    /** OS'a en son başarıyla register edilmiş fence requestId'leri (teşhis). */
    fun registeredFenceRequestIds(): List<String> = registeredIdsStore.load()

    fun removeAll(onComplete: () -> Unit = {}) {
        registeredIdsStore.save(emptyList())
        // Eski modül (v1) ile oluşturulmuş leftover geofence'leri de temizle (migration cleanup).
        // v1, geofence'lerini kendi PendingIntent'leriyle kaydettiği için v2'nin removeGeofences'ı
        // onları silmez; ayrı ayrı v1 PendingIntent'leriyle silinir.
        removeLegacyV1Geofences()
        client.removeGeofences(pendingIntent())
            .addOnCompleteListener { onComplete() }
    }

    private fun removeLegacyV1Geofences() {
        try {
            client.removeGeofences(GeofenceLocationReceiver.getSyncedGeofencesPendingIntent(context))
            client.removeGeofences(GeofenceLocationReceiver.getBubbleGeofencePendingIntent(context))
            DengageLogger.debug("OsGeofenceRegistrar -> removed legacy v1 geofences")
        } catch (e: Exception) {
            DengageLogger.error("OsGeofenceRegistrar -> legacy v1 cleanup failed: ${e.message}")
        }
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

/**
 * OS'a register edilmiş fence requestId'lerini kalıcı tutar.
 *
 * Android'de `GeofencingClient` kayıtlı geofence'leri sorgulayacak bir API sunmuyor
 * (iOS'taki `CLLocationManager.monitoredRegions` muadili yok), bu yüzden teşhis amacıyla
 * ne register ettiğimizi kendimiz kaydediyoruz. Kaynak "OS'un gerçeği" değil, "bizim en son
 * başarılı register'ımız" — process yeniden başlasa da doğru kalması için persist edilir.
 */
private class RegisteredFenceIdsStore(context: Context) {

    private val prefs by lazy {
        context.applicationContext.getSharedPreferences(PREFS_FILE, Context.MODE_PRIVATE)
    }

    fun save(requestIds: List<String>) {
        prefs.edit().putStringSet(KEY_IDS, requestIds.toSet()).apply()
    }

    fun load(): List<String> =
        prefs.getStringSet(KEY_IDS, emptySet())?.toList().orEmpty()

    companion object {
        private const val PREFS_FILE = "dengage_geofence_engine_prefs"
        private const val KEY_IDS = "registered_fence_request_ids"
    }
}
