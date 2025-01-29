package com.dengage.geofence.manager

import android.annotation.SuppressLint
import android.app.PendingIntent
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.os.Build
import com.dengage.geofence.DengageGeofence
import com.dengage.sdk.Dengage
import com.dengage.sdk.domain.geofence.model.GeofenceLocationSource
import com.dengage.sdk.util.DengageLogger
import com.google.android.gms.location.Geofence
import com.google.android.gms.location.GeofencingEvent
import com.google.android.gms.location.LocationResult

class GeofenceLocationReceiver: BroadcastReceiver() {

    internal companion object {

        internal const val ACTION_LOCATION = "com.dengage.geofence.manager.GeofenceLocationReceiver.LOCATION"
        internal const val ACTION_BUBBLE_GEOFENCE = "com.dengage.geofence.manager.GeofenceLocationReceiver.GEOFENCE"
        internal const val ACTION_SYNCED_GEOFENCES = "com.dengage.geofence.manager.GeofenceLocationReceiver.SYNCED_GEOFENCES"

        private const val REQUEST_CODE_LOCATION = 192004230
        private const val REQUEST_CODE_BUBBLE_GEOFENCE = 192004231
        private const val REQUEST_CODE_SYNCED_GEOFENCES = 192004232

        internal fun getLocationPendingIntent(context: Context): PendingIntent {
            val intent = baseIntent(context).apply {
                action = ACTION_LOCATION
            }
            val flags = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
                PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_MUTABLE
            } else {
                PendingIntent.FLAG_UPDATE_CURRENT
            }
            return PendingIntent.getBroadcast(
                context,
                REQUEST_CODE_LOCATION,
                intent,
                flags
            )
        }

        internal fun getBubbleGeofencePendingIntent(context: Context): PendingIntent {
            val intent = baseIntent(context).apply {
                action = ACTION_BUBBLE_GEOFENCE
            }
            val flags = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
                PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_MUTABLE
            } else {
                PendingIntent.FLAG_UPDATE_CURRENT
            }
            return PendingIntent.getBroadcast(
                context,
                REQUEST_CODE_BUBBLE_GEOFENCE,
                intent,
                flags
            )
        }

        internal fun getSyncedGeofencesPendingIntent(context: Context): PendingIntent {
            val intent = baseIntent(context).apply {
                action = ACTION_SYNCED_GEOFENCES
            }
            val flags = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
                PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_MUTABLE
            } else {
                PendingIntent.FLAG_UPDATE_CURRENT
            }
            return PendingIntent.getBroadcast(
                context,
                REQUEST_CODE_SYNCED_GEOFENCES,
                intent,
                flags
            )
        }

        private fun baseIntent(context: Context): Intent = Intent(context, GeofenceLocationReceiver::class.java)

    }

    @SuppressLint("MissingPermission")
    override fun onReceive(context: Context, intent: Intent) {
        if (!Dengage.initialized) {
            Dengage.init(context = context, initForGeofence = true)
        }

        DengageLogger.debug("Received broadcast | action = ${intent.action}")

        when (intent.action) {
            ACTION_BUBBLE_GEOFENCE, ACTION_SYNCED_GEOFENCES -> {
                val event = GeofencingEvent.fromIntent(intent)
                val triggeredGeofences = event?.triggeringGeofences
                if (triggeredGeofences != null) {
                    for(triggeredGeofence in triggeredGeofences) {
                        val source = when (event.geofenceTransition) {
                            Geofence.GEOFENCE_TRANSITION_ENTER -> GeofenceLocationSource.GEOFENCE_ENTER
                            Geofence.GEOFENCE_TRANSITION_DWELL -> GeofenceLocationSource.GEOFENCE_DWELL
                            else -> GeofenceLocationSource.GEOFENCE_EXIT
                        }

                        event.triggeringLocation?.let {
                            DengageGeofence.handleLocation(context,
                                it, source, triggeredGeofence.requestId)
                        }

                        /*
                                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                                    GeofenceJobScheduler.scheduleJob(context, event.triggeringLocation, source, triggeredGeofence.requestId)
                                } else {

                                }
                                 */

                    }
                }
            }
            ACTION_LOCATION -> {
                if(!LocationResult.hasResult(intent)) {
                    return
                }
                val result = LocationResult.extractResult(intent)
                result?.lastLocation.also {
                    val source = GeofenceLocationSource.BACKGROUND_LOCATION

                    if (it != null) {
                        DengageGeofence.handleLocation(context, it, source, null)
                    }

                    /*
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                        GeofenceJobScheduler.scheduleJob(context, it, source, null)
                    } else {
                        Dengage.handleLocation(context, it, source, null)
                    }

                     */

                }
            }
            Intent.ACTION_BOOT_COMPLETED -> {
                DengageGeofence.handleBootCompleted(context)
            }
        }
    }

}