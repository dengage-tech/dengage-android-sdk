package com.dengage.sdk.domain.geofence.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.geofence.GeofenceRepository
import com.dengage.sdk.domain.geofence.model.GeofenceCluster
import com.dengage.sdk.util.createLazy

class GetGeofenceClusters : CoroutineUseCase<Array<GeofenceCluster>?, GetGeofenceClusters.Params>() {

    private val repository: GeofenceRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Array<GeofenceCluster>? =
        repository.getGeofences(
            integrationKey = params!!.integrationKey,
            latitude = params.latitude,
            longitude = params.longitude
        )

    data class Params(
        val integrationKey: String,
        val latitude: Double,
        val longitude: Double
    )
}
