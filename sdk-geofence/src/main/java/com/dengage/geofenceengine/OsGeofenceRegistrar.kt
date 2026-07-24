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

    /**
     * Register modu:
     * - [DIFF]: kayıtlı set ile hedef set karşılaştırılır; yalnız çıkan/eklenen/değişen fence'lere
     *   dokunulur. Değişmeyen fence korunur → OS'un dwell loiter timer'ı sıfırlanmaz, unregister
     *   penceresinde exit kaçmaz. Organic sync / movement / active-window yolu bunu kullanır.
     * - [FULL]: remove-all + tam re-register (eski davranış). Tamir kanalı: boot, forceResync,
     *   silent push, PROVIDERS_CHANGED — store OS'un gerçeğinden sapmış olabilir, diff'e güvenilmez.
     */
    enum class RegisterMode { DIFF, FULL }

    fun register(fences: List<Fence>, mode: RegisterMode = RegisterMode.DIFF) {
        if (mode == RegisterMode.FULL) {
            fullRegister(fences)
            return
        }

        // requestId -> (OS geofence, içerik parmak izi)
        val targetById: Map<String, Pair<Geofence, String>> = fences
            .mapNotNull { fence -> buildGeofence(fence)?.let { it.requestId to (it to fingerprint(fence)) } }
            .toMap()
        val registered = registeredIdsStore.loadFingerprints()

        val toRemove = (registered.keys - targetById.keys).toList()
        // Yeni VEYA içeriği değişmiş fence'ler: add yeterli — GMS aynı requestId'yi replace eder.
        val toAdd = targetById
            .filter { (requestId, entry) -> registered[requestId] != entry.second }
            .map { it.value.first }

        if (toRemove.isEmpty() && toAdd.isEmpty()) {
            DengageLogger.debug("OsGeofenceRegistrar -> diff no-op (${targetById.size} fences unchanged)")
            return
        }
        DengageLogger.debug(
            "OsGeofenceRegistrar -> diff: +${toAdd.size} -${toRemove.size} =${targetById.size - toAdd.size}"
        )

        val afterRemove: () -> Unit = afterRemove@{
            val survivors = (registered - toRemove.toSet()).toMutableMap()
            if (toAdd.isEmpty()) {
                registeredIdsStore.saveFingerprints(survivors)
                return@afterRemove
            }
            val request = GeofencingRequest.Builder()
                .setInitialTrigger(GeofencingRequest.INITIAL_TRIGGER_ENTER)
                .addGeofences(toAdd)
                .build()
            client.addGeofences(request, pendingIntent())
                .addOnSuccessListener {
                    toAdd.forEach { survivors[it.requestId] = targetById.getValue(it.requestId).second }
                    registeredIdsStore.saveFingerprints(survivors)
                    DengageLogger.debug("OsGeofenceRegistrar -> diff registered ${toAdd.size} fences")
                }
                .addOnFailureListener { e ->
                    // Eklenen/değişenler store'a girmez → sonraki diff yeniden dener (self-healing).
                    toAdd.forEach { survivors.remove(it.requestId) }
                    registeredIdsStore.saveFingerprints(survivors)
                    DengageLogger.error("OsGeofenceRegistrar -> diff register failed: ${e.message}")
                    GeofenceDebugLogger.error(
                        "Geofence OS diff register failed",
                        mapOf("error" to (e.message ?: "unknown"), "add" to toAdd.size.toString())
                    )
                }
        }

        if (toRemove.isEmpty()) {
            afterRemove()
        } else {
            // DİKKAT: List<String> overload — PendingIntent overload'ı TÜM geofence'leri silerdi.
            client.removeGeofences(toRemove).addOnCompleteListener { afterRemove() }
        }
    }

    /** Tamir kanalı: remove-all + tam re-register (önceki `register` davranışı). */
    private fun fullRegister(fences: List<Fence>) {
        removeAll {
            if (fences.isEmpty()) {
                DengageLogger.debug("OsGeofenceRegistrar -> nothing to register")
                return@removeAll
            }
            val entries = fences.mapNotNull { fence ->
                buildGeofence(fence)?.let { it to fingerprint(fence) }
            }
            if (entries.isEmpty()) return@removeAll
            val osGeofences = entries.map { it.first }

            val request = GeofencingRequest.Builder()
                .setInitialTrigger(GeofencingRequest.INITIAL_TRIGGER_ENTER)
                .addGeofences(osGeofences)
                .build()

            client.addGeofences(request, pendingIntent())
                .addOnSuccessListener {
                    DengageLogger.debug("OsGeofenceRegistrar -> registered ${osGeofences.size} fences")
                    // GeofencingClient kayıtlı geofence'leri sorgulama API'si sunmuyor
                    // (iOS'taki `monitoredRegions` muadili yok) → diff ve teşhis için kendimiz tutuyoruz.
                    registeredIdsStore.saveFingerprints(
                        entries.associate { it.first.requestId to it.second }
                    )
                }
                .addOnFailureListener {
                    DengageLogger.error("OsGeofenceRegistrar -> register failed: ${it.message}")
                    registeredIdsStore.saveFingerprints(emptyMap())
                    GeofenceDebugLogger.error(
                        "Geofence OS register failed",
                        mapOf("error" to (it.message ?: "unknown"), "count" to osGeofences.size.toString())
                    )
                }
        }
    }

    /**
     * İçerik parmak izi: geometri + dwell süresi. requestId aynı kalıp bunlardan biri değişirse
     * OS'taki tanım bayattır → diff yeniden add eder (GMS aynı requestId'yi replace eder).
     */
    private fun fingerprint(fence: Fence): String {
        val dwellMinutes = fence.campaigns
            .filter { it.triggerType == GeofenceTriggerType.DWELL }
            .mapNotNull { it.dwellMinutes }
            .maxOrNull() ?: 0
        return "${fence.latitude}|${fence.longitude}|${fence.radiusM}|$dwellMinutes"
    }

    /** OS'a en son başarıyla register edilmiş fence requestId'leri (teşhis). */
    fun registeredFenceRequestIds(): List<String> = registeredIdsStore.load()

    fun removeAll(onComplete: () -> Unit = {}) {
        registeredIdsStore.saveFingerprints(emptyMap())
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
 * OS'a register edilmiş fence requestId'lerini + içerik parmak izlerini kalıcı tutar.
 *
 * Android'de `GeofencingClient` kayıtlı geofence'leri sorgulayacak bir API sunmuyor
 * (iOS'taki `CLLocationManager.monitoredRegions` muadili yok), bu yüzden diff-based register ve
 * teşhis için ne register ettiğimizi kendimiz kaydediyoruz. Kaynak "OS'un gerçeği" değil, "bizim
 * en son başarılı register'ımız" — process yeniden başlasa da doğru kalması için persist edilir.
 * Store OS'tan sapabilir (reboot, konum toggle, GMS güncellemesi); bu yüzden tamir kanalları
 * (boot / forceResync / PROVIDERS_CHANGED) diff'i atlayıp FULL register yapar.
 *
 * Format: `"requestId=fingerprint"` string set'i. Eski `registered_fence_request_ids` anahtarı
 * okunmaz; ilk diff store'u boş görür ve tüm set'i add eder (zararsız migration).
 */
private class RegisteredFenceIdsStore(context: Context) {

    private val prefs by lazy {
        context.applicationContext.getSharedPreferences(PREFS_FILE, Context.MODE_PRIVATE)
    }

    fun saveFingerprints(map: Map<String, String>) {
        prefs.edit()
            .putStringSet(KEY_FINGERPRINTS, map.map { "${it.key}=${it.value}" }.toSet())
            .apply()
    }

    fun loadFingerprints(): Map<String, String> =
        prefs.getStringSet(KEY_FINGERPRINTS, emptySet()).orEmpty()
            .mapNotNull { entry ->
                val idx = entry.indexOf('=')
                if (idx <= 0) null else entry.substring(0, idx) to entry.substring(idx + 1)
            }
            .toMap()

    fun load(): List<String> = loadFingerprints().keys.toList()

    companion object {
        private const val PREFS_FILE = "dengage_geofence_engine_prefs"
        private const val KEY_FINGERPRINTS = "registered_fence_fingerprints"
    }
}
