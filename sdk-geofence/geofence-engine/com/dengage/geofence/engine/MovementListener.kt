package com.dengage.geofence.engine

import android.annotation.SuppressLint
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.os.Build
import com.dengage.geofence.engine.receiver.GeofenceBroadcastReceiver
import com.dengage.sdk.util.DengageLogger
import com.google.android.gms.location.FusedLocationProviderClient
import com.google.android.gms.location.LocationRequest
import com.google.android.gms.location.LocationServices
import com.google.android.gms.location.Priority

/**
 * Hareket dinleyici (doc 21 §6.5). `FusedLocationProviderClient` + `setMinUpdateDistanceMeters`.
 * FLP cell+WiFi tabanlı konum verebildiği için uçak modu sonrası ilk fix mobil veri gerektirmez;
 * hücresel/WiFi yokken GPS-only fallback'e devam eder.
 *
 * Konum güncellemeleri PendingIntent ile [GeofenceBroadcastReceiver]'a (ACTION_LOCATION) gelir —
 * background/kill-state'te güvenilir teslimat için.
 */
@SuppressLint("MissingPermission")
class MovementListener(private val context: Context) {

    private val client: FusedLocationProviderClient =
        LocationServices.getFusedLocationProviderClient(context)

    @Volatile
    private var active = false
    private var activeMinDistance = -1

    val isActive: Boolean get() = active

    fun start(minUpdateDistanceMeters: Int, intervalMillis: Long = DEFAULT_INTERVAL_MS) {
        if (active && activeMinDistance == minUpdateDistanceMeters) return
        stop()
        val request = LocationRequest.Builder(Priority.PRIORITY_BALANCED_POWER_ACCURACY, intervalMillis)
            .setMinUpdateDistanceMeters(minUpdateDistanceMeters.toFloat())
            .setMinUpdateIntervalMillis(FASTEST_INTERVAL_MS)
            .build()
        client.requestLocationUpdates(request, pendingIntent())
            .addOnSuccessListener {
                active = true
                activeMinDistance = minUpdateDistanceMeters
                DengageLogger.debug("MovementListener -> started (minDistance=${minUpdateDistanceMeters}m)")
            }
            .addOnFailureListener {
                DengageLogger.error("MovementListener -> start failed: ${it.message}")
            }
    }

    fun stop() {
        client.removeLocationUpdates(pendingIntent())
        active = false
        activeMinDistance = -1
        DengageLogger.debug("MovementListener -> stopped")
    }

    private fun pendingIntent(): PendingIntent {
        val intent = Intent(context, GeofenceBroadcastReceiver::class.java)
            .setAction(GeofenceBroadcastReceiver.ACTION_LOCATION_UPDATE)
        var flags = PendingIntent.FLAG_UPDATE_CURRENT
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            flags = flags or PendingIntent.FLAG_MUTABLE
        }
        return PendingIntent.getBroadcast(context, REQUEST_CODE, intent, flags)
    }

    companion object {
        private const val REQUEST_CODE = 0xD6F3
        private const val DEFAULT_INTERVAL_MS = 150_000L
        private const val FASTEST_INTERVAL_MS = 30_000L
    }
}
