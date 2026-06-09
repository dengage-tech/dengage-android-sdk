package com.dengage.sdk.domain.geofence

import com.dengage.sdk.domain.geofence.model.GeofenceCluster
import com.dengage.sdk.domain.geofence.model.GeofenceEventSignalRequest
import com.dengage.sdk.domain.geofence.model.sync.DeviceHeartbeatRequest
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventSignalRequestV2
import com.dengage.sdk.domain.geofence.model.sync.GeofenceSyncResponse
import retrofit2.Response
import retrofit2.http.*

interface GeofenceService {

    // ---- v1 (legacy, mevcut manager ile coexist) ----

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

    // ---- v2 (Geofence Engine, contract) ----

    /**
     * `GET /geofences/sync/{integrationKey}` — full payload + ETag (contract §1).
     * `Response<>` döner ki SDK ETag header'ını okuyabilsin ve 304'ü ayırt edebilsin.
     */
    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @GET("geofences/sync/{integrationKey}")
    suspend fun getGeofenceSync(
        @Path("integrationKey") integrationKey: String,
        @Query("deviceId") deviceId: String?,
        @Query("contactKey") contactKey: String?,
        @Query("lat") lat: Double?,
        @Query("lon") lon: Double?,
        @Header("If-None-Match") ifNoneMatch: String?
    ): Response<GeofenceSyncResponse>

    /** `POST /devices/heartbeat/{integrationKey}` — device_last_location UPSERT (contract §2). */
    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @POST("devices/heartbeat/{integrationKey}")
    suspend fun sendDeviceHeartbeat(
        @Path("integrationKey") integrationKey: String,
        @Body request: DeviceHeartbeatRequest
    ): Response<Unit>

    /** `POST /event-signal/{integrationKey}` v2 — trigger + replay + dedup (contract §3). */
    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @POST("event-signal/{integrationKey}")
    suspend fun sendGeofenceEventSignalV2(
        @Path("integrationKey") integrationKey: String,
        @Body request: GeofenceEventSignalRequestV2
    ): Response<Unit>
}
