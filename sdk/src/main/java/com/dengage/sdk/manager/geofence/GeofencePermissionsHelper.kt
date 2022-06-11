package com.dengage.sdk.manager.geofence

import android.Manifest
import android.app.Activity
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.os.Build
import androidx.core.content.ContextCompat
import com.dengage.sdk.domain.geofence.model.DengageLocationPermission

object GeofencePermissionsHelper {

    internal fun fineLocationPermission(context: Context): Boolean {
        return ContextCompat.checkSelfPermission(
            context,
            Manifest.permission.ACCESS_FINE_LOCATION
        ) == PackageManager.PERMISSION_GRANTED
    }

    internal fun coarseLocationPermission(context: Context): Boolean {
        return ContextCompat.checkSelfPermission(
            context,
            Manifest.permission.ACCESS_COARSE_LOCATION
        ) == PackageManager.PERMISSION_GRANTED
    }

    internal fun requestLocationPermissions(activity: Activity) {
        val intent = Intent(activity, GeofencePermissionActivity::class.java)
        activity.startActivity(intent)
    }

    internal fun checkPermissions(activity: Activity): Boolean {
        val applicationContext = activity.applicationContext
        val fineLocationPermissionState = ContextCompat.checkSelfPermission(applicationContext,
            Manifest.permission.ACCESS_FINE_LOCATION
        )

        val backgroundLocationPermissionState = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q)
            ContextCompat.checkSelfPermission(applicationContext,
                Manifest.permission.ACCESS_BACKGROUND_LOCATION
            )
        else PackageManager.PERMISSION_GRANTED

        return fineLocationPermissionState == PackageManager.PERMISSION_GRANTED &&
                backgroundLocationPermissionState == PackageManager.PERMISSION_GRANTED
    }

    internal fun getLocationPermissionStatus(context: Context): DengageLocationPermission {
        return if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.P) {
            if (ContextCompat.checkSelfPermission(
                    context,
                    Manifest.permission.ACCESS_FINE_LOCATION
                )
                == PackageManager.PERMISSION_GRANTED ||
                ContextCompat.checkSelfPermission(
                    context,
                    Manifest.permission.ACCESS_COARSE_LOCATION
                )
                == PackageManager.PERMISSION_GRANTED
            ) {
                DengageLocationPermission.ALWAYS
            } else {
                DengageLocationPermission.NONE
            }
        } else {
            if (ContextCompat.checkSelfPermission(
                    context,
                    Manifest.permission.ACCESS_BACKGROUND_LOCATION
                )
                == PackageManager.PERMISSION_GRANTED
            ) {
                DengageLocationPermission.ALWAYS
            } else {
                if (ContextCompat.checkSelfPermission(
                        context,
                        Manifest.permission.ACCESS_FINE_LOCATION
                    )
                    == PackageManager.PERMISSION_GRANTED ||
                    ContextCompat.checkSelfPermission(
                        context,
                        Manifest.permission.ACCESS_COARSE_LOCATION
                    )
                    == PackageManager.PERMISSION_GRANTED
                ) {
                    DengageLocationPermission.APP_OPEN
                } else {
                    DengageLocationPermission.NONE
                }
            }
        }
    }

}