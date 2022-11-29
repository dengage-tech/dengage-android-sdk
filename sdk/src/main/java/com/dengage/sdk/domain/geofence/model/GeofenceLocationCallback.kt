package com.dengage.sdk.domain.geofence.model

import android.location.Location

interface GeofenceLocationCallback  {

    fun onComplete(
        status: GeofenceStatus,
        location: Location? = null,
        stopped: Boolean = false
    )

}
