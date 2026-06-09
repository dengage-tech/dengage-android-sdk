package com.dengage.geofence.engine

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.model.GeofenceConfiguration

/**
 * Geofence tuning config'ini merkezi `SdkParameters.geofence` bloğundan okur ve clamp eder
 * (contract §4). Sync response'ta tuning yer almaz; bu yüzden config buradan gelir.
 */
class RemoteConfigClient {

    /** Her zaman clamp'lenmiş bir config döner; server config yoksa default'lar kullanılır. */
    fun config(): GeofenceConfiguration =
        (Prefs.sdkParameters?.geofence ?: GeofenceConfiguration()).clamped()

    fun geofenceEnabled(): Boolean = Prefs.sdkParameters?.geofenceEnabled ?: true
}
