package com.dengage.geofence.manager

import android.Manifest
import android.app.Activity
import android.content.pm.PackageManager
import android.os.Build
import android.os.Bundle
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.geofence.model.DengageLocationPermission
import com.dengage.sdk.util.EdgeToEdgeUtils

class GeofencePermissionActivity : Activity() {
    companion object {
        private const val LOCATION_PERMISSION_REQUEST_CODE = 19000001
        private const val BACKGROUND_LOCATION_PERMISSION_REQUEST_CODE = 19000002
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        
        val accessFineLocationPermission = ContextCompat.checkSelfPermission(
            this,
            Manifest.permission.ACCESS_FINE_LOCATION
        ) == PackageManager.PERMISSION_GRANTED
        val accessCoarseLocationPermission = ContextCompat.checkSelfPermission(
            this,
            Manifest.permission.ACCESS_COARSE_LOCATION
        ) == PackageManager.PERMISSION_GRANTED
        if (!(accessFineLocationPermission || accessCoarseLocationPermission)) {
            ActivityCompat.requestPermissions(
                this, arrayOf(
                    Manifest.permission.ACCESS_FINE_LOCATION,
                    Manifest.permission.ACCESS_COARSE_LOCATION
                ),
                LOCATION_PERMISSION_REQUEST_CODE
            )
        } else {
            val locationPermission: DengageLocationPermission =
                GeofencePermissionsHelper.getLocationPermissionStatus(this)
            if (locationPermission != DengageLocationPermission.ALWAYS) {
                if (Build.VERSION.SDK_INT > Build.VERSION_CODES.P) {
                    ActivityCompat.requestPermissions(
                        this, arrayOf(Manifest.permission.ACCESS_BACKGROUND_LOCATION),
                        BACKGROUND_LOCATION_PERMISSION_REQUEST_CODE
                    )
                }
            } else {
                finish()
            }
        }
    }

    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<String?>,
        grantResults: IntArray
    ) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
        if (requestCode == LOCATION_PERMISSION_REQUEST_CODE) {
            if (grantResults.isNotEmpty() && grantResults[0] == PackageManager.PERMISSION_GRANTED ||
                grantResults.size > 1 && grantResults[1] == PackageManager.PERMISSION_GRANTED
            ) {
                val locationPermission: DengageLocationPermission =
                    GeofencePermissionsHelper.getLocationPermissionStatus(this)
                if (locationPermission != DengageLocationPermission.ALWAYS) {
                    if (Build.VERSION.SDK_INT > Build.VERSION_CODES.P) {
                        ActivityCompat.requestPermissions(
                            this, arrayOf(Manifest.permission.ACCESS_BACKGROUND_LOCATION),
                            BACKGROUND_LOCATION_PERMISSION_REQUEST_CODE
                        )
                    }
                }
            } else {
                finish()
            }
        } else if (requestCode == BACKGROUND_LOCATION_PERMISSION_REQUEST_CODE) {
            if (grantResults.isNotEmpty() && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                finish()
                if(Prefs.geofenceEnabled) {
                    //Dengage.startGeofence() //TODO:gerek var mı?
                }
            } else {
                finish()
            }
        }
    }
}