package com.dengage.geofence.engine.storage.model

import com.dengage.sdk.domain.geofence.model.sync.SyncCampaign
import com.dengage.sdk.domain.geofence.model.sync.SyncFence

/**
 * Engine-içi flattened fence modeli. Sync response'taki [SyncFence] -> [Fence] map edilir,
 * SQLite + R*Tree storage'a persist edilir. Kampanyalar JSON blob olarak saklanır.
 */
data class Fence(
    val fenceId: Int,
    val clusterId: Int,
    val latitude: Double,
    val longitude: Double,
    val radiusM: Double,
    val title: String?,
    val activeNow: Boolean,
    val nextStateChangeAtMillis: Long?,
    val nextStateChangeTo: String?,
    val campaigns: List<SyncCampaign>
) {
    /** OS register'da kullanılacak benzersiz requestId. */
    val requestId: String
        get() = requestId(clusterId, fenceId)

    companion object {
        fun from(sync: SyncFence): Fence = Fence(
            fenceId = sync.fenceId,
            clusterId = sync.clusterId,
            latitude = sync.latitude,
            longitude = sync.longitude,
            radiusM = sync.radiusM,
            title = sync.title,
            activeNow = sync.activeNow,
            nextStateChangeAtMillis = IsoTime.parseOrNull(sync.nextStateChangeAt),
            nextStateChangeTo = sync.nextStateChangeTo,
            campaigns = sync.campaigns
        )

        fun requestId(clusterId: Int, fenceId: Int): String =
            "${com.dengage.sdk.util.Constants.GEOFENCE_SYNC_PREFIX}_${clusterId}_${fenceId}"

        /** requestId'den (clusterId, fenceId) çıkarır; geçersizse null. */
        fun parseRequestId(requestId: String?): Pair<Int, Int>? {
            if (requestId == null) return null
            if (!requestId.startsWith(com.dengage.sdk.util.Constants.GEOFENCE_SYNC_PREFIX)) return null
            val parts = requestId.split("_")
            if (parts.size < 3) return null
            val clusterId = parts[parts.size - 2].toIntOrNull() ?: return null
            val fenceId = parts[parts.size - 1].toIntOrNull() ?: return null
            return clusterId to fenceId
        }
    }
}
