package com.dengage.geofence.engine

import com.dengage.geofence.engine.storage.SyncMetadataRepository
import com.dengage.geofence.engine.storage.FenceRepository
import com.dengage.geofence.engine.storage.model.Fence
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.geofence.GeofenceRepository
import com.dengage.sdk.util.DengageLogger

/**
 * `GET /geofences/sync` çağrısı + ETag yönetimi + storage persist (contract §1, doc 21 §6.5).
 * 304 alınırsa mevcut cache korunur.
 */
class GeofenceSyncer(
    private val fenceRepository: FenceRepository,
    private val syncMetadata: SyncMetadataRepository,
    private val apiRepository: GeofenceRepository = GeofenceRepository()
) {

    sealed class SyncResult {
        data class Updated(val fences: List<Fence>) : SyncResult()
        object NotModified : SyncResult()
        data class Error(val throwable: Throwable?) : SyncResult()
        object NoSubscription : SyncResult()
    }

    suspend fun sync(lat: Double?, lon: Double?): SyncResult {
        val subscription = Prefs.subscription ?: return SyncResult.NoSubscription
        val integrationKey = subscription.integrationKey
        if (integrationKey.isBlank()) return SyncResult.NoSubscription

        return try {
            val response = apiRepository.getGeofenceSync(
                integrationKey = integrationKey,
                deviceId = subscription.deviceId,
                contactKey = subscription.contactKey,
                latitude = lat,
                longitude = lon,
                ifNoneMatch = syncMetadata.lastETag
            )

            when {
                response.code() == 304 -> {
                    DengageLogger.debug("GeofenceSyncer -> 304 Not Modified, keeping cache")
                    syncMetadata.lastSyncedAt = System.currentTimeMillis()
                    SyncResult.NotModified
                }

                response.isSuccessful -> {
                    val body = response.body()
                    if (body == null) {
                        DengageLogger.debug("GeofenceSyncer -> empty body")
                        return SyncResult.Error(null)
                    }
                    val fences = body.geofences.map { Fence.from(it) }
                    fenceRepository.save(fences)

                    // ETag öncelik: HTTP header > body.etag
                    val etag = response.headers()["ETag"] ?: body.etag
                    syncMetadata.lastETag = etag
                    syncMetadata.lastSyncedAt = System.currentTimeMillis()
                    DengageLogger.debug("GeofenceSyncer -> synced ${fences.size} fences, etag=$etag")
                    SyncResult.Updated(fences)
                }

                else -> {
                    DengageLogger.error("GeofenceSyncer -> http ${response.code()}")
                    SyncResult.Error(null)
                }
            }
        } catch (e: Exception) {
            DengageLogger.error("GeofenceSyncer -> error ${e.message}")
            SyncResult.Error(e)
        }
    }
}
