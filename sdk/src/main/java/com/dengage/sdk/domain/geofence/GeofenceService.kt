package com.dengage.sdk.domain.geofence

import com.dengage.sdk.domain.geofence.model.GeofenceCluster
import com.dengage.sdk.domain.geofence.model.GeofenceEventSignalRequest
import retrofit2.Response
import retrofit2.http.*

interface GeofenceService {

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("geofence/{integrationKey}")
    suspend fun getGeofences(
        @Path("integrationKey") integrationKey: String,
        @Query("latitude") latitude: Double,
        @Query("longitude") longitude: Double
    ): Array<GeofenceCluster>?

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @POST("event-signal/{integrationKey}")
    suspend fun sendGeofenceEventSignal(
        @Path("integrationKey") integrationKey: String,
        @Body request: GeofenceEventSignalRequest
    ): Response<Unit>

}