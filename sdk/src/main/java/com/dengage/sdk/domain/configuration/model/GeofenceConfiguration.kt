package com.dengage.sdk.domain.configuration.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

/**
 * Geofence tuning config'i. Sync response'ta DEĞİL; merkezi `SdkParameters.geofence` bloğunda gelir
 * (contract §4). SDK okurken aralık dışı değerleri min/max'a clamp eder ([clamped]).
 */
data class GeofenceConfiguration(
    @SerializedName("reevaluationDistanceMeters") val reevaluationDistanceMeters: Int = DEFAULT_REEVAL_DISTANCE,
    @SerializedName("topN") val topN: Int = DEFAULT_TOP_N,
    @SerializedName("syncHorizonHours") val syncHorizonHours: Int = DEFAULT_SYNC_HORIZON_HOURS,
    @SerializedName("heartbeatIntervalMinutes") val heartbeatIntervalMinutes: Int = DEFAULT_HEARTBEAT_INTERVAL_MIN,
    @SerializedName("offlineQueueMaxSize") val offlineQueueMaxSize: Int = DEFAULT_OFFLINE_QUEUE_MAX,
    @SerializedName("adaptiveThreshold") val adaptiveThreshold: AdaptiveThresholdConfig = AdaptiveThresholdConfig(),
    @SerializedName("wakeupCap") val wakeupCap: WakeupCapConfig = WakeupCapConfig()
) : Serializable {

    /** Tüm alanları contract §4 min/max aralığına clamp edilmiş bir kopya döner. */
    fun clamped(): GeofenceConfiguration = copy(
        reevaluationDistanceMeters = reevaluationDistanceMeters.coerceIn(100, 50000),
        topN = topN.coerceIn(5, 100),
        syncHorizonHours = syncHorizonHours.coerceIn(12, 168),
        heartbeatIntervalMinutes = heartbeatIntervalMinutes.coerceIn(15, 1440),
        offlineQueueMaxSize = offlineQueueMaxSize.coerceIn(10, 1000),
        adaptiveThreshold = adaptiveThreshold.clamped(),
        wakeupCap = wakeupCap.clamped()
    )

    companion object {
        const val DEFAULT_REEVAL_DISTANCE = 1000
        const val DEFAULT_TOP_N = 20
        const val DEFAULT_SYNC_HORIZON_HOURS = 25
        const val DEFAULT_HEARTBEAT_INTERVAL_MIN = 60
        const val DEFAULT_OFFLINE_QUEUE_MAX = 100
    }
}

data class AdaptiveThresholdConfig(
    @SerializedName("tiers") val tiers: List<AdaptiveTier> = defaultTiers(),
    @SerializedName("cooldownSeconds") val cooldownSeconds: Int = 60,
    @SerializedName("transitModeEntryMinutes") val transitModeEntryMinutes: Int = 5,
    @SerializedName("transitModeEntrySpeedKmh") val transitModeEntrySpeedKmh: Int = 20,
    @SerializedName("transitModeExitMinutes") val transitModeExitMinutes: Int = 3,
    @SerializedName("transitModeExitSpeedKmh") val transitModeExitSpeedKmh: Int = 5
) : Serializable {

    fun clamped(): AdaptiveThresholdConfig = copy(
        tiers = if (tiers.isEmpty()) defaultTiers() else tiers.sortedBy { it.minSpeedKmh },
        cooldownSeconds = cooldownSeconds.coerceIn(10, 600),
        transitModeEntryMinutes = transitModeEntryMinutes.coerceIn(1, 30),
        transitModeEntrySpeedKmh = transitModeEntrySpeedKmh.coerceIn(5, 100),
        transitModeExitMinutes = transitModeExitMinutes.coerceIn(1, 30),
        transitModeExitSpeedKmh = transitModeExitSpeedKmh.coerceIn(1, 50)
    )

    companion object {
        fun defaultTiers(): List<AdaptiveTier> = listOf(
            AdaptiveTier(0, 1000),
            AdaptiveTier(20, 2000),
            AdaptiveTier(60, 5000),
            AdaptiveTier(150, 20000)
        )
    }
}

data class AdaptiveTier(
    @SerializedName("minSpeedKmh") val minSpeedKmh: Int,
    @SerializedName("thresholdMeters") val thresholdMeters: Int
) : Serializable

data class WakeupCapConfig(
    @SerializedName("hourlyMax") val hourlyMax: Int = 6,
    @SerializedName("pauseMinutes") val pauseMinutes: Int = 30,
    @SerializedName("slidingWindowMinutes") val slidingWindowMinutes: Int = 60
) : Serializable {

    fun clamped(): WakeupCapConfig = copy(
        hourlyMax = hourlyMax.coerceIn(3, 30),
        pauseMinutes = pauseMinutes.coerceIn(5, 240),
        slidingWindowMinutes = slidingWindowMinutes.coerceIn(30, 360)
    )
}
