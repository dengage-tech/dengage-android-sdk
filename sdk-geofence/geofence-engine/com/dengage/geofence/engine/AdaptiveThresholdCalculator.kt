package com.dengage.geofence.engine

import android.location.Location
import com.dengage.sdk.domain.configuration.model.AdaptiveThresholdConfig
import com.dengage.sdk.util.DengageLogger
import java.util.ArrayDeque

/**
 * Adaptif hareket eşiği (K12, doc 19 §12 / doc 21 §6.5).
 * Cihazın anlık hızına göre reeval mesafe eşiğini ayarlar; cooldown ve transit modu uygular.
 *
 * Thread-safety: tek bir background thread'den (MovementListener callback) çağrılması beklenir;
 * yine de iç state senkronize edilir.
 */
class AdaptiveThresholdCalculator(
    private val configProvider: () -> AdaptiveThresholdConfig
) {

    private data class SpeedSample(val timestamp: Long, val speedKmh: Double)

    private val speedSamples = ArrayDeque<SpeedSample>()
    private var lastReevalAt: Long = 0
    private var transitModeActive: Boolean = false
    private var transitModeEnterCandidateSince: Long? = null
    private var transitModeExitCandidateSince: Long? = null

    @Synchronized
    fun shouldReeval(
        current: Location,
        lastReevalLocation: Location?,
        now: Long = System.currentTimeMillis()
    ): ReevalDecision {
        val config = configProvider()

        // 1. Anlık hız örneği (m/s -> km/h). speed geçersizse 0 say.
        val speedKmh = computeSpeedKmh(current)
        speedSamples.addLast(SpeedSample(now, speedKmh))
        purgeOldSamples(now, config)

        // 2. Cooldown — ardışık reeval'ler arası min süre
        if (lastReevalAt != 0L && (now - lastReevalAt) < config.cooldownSeconds * 1000L) {
            return ReevalDecision.Skip(SkipReason.COOLDOWN, transitModeActive)
        }

        // 3. Transit modu değerlendirmesi
        evaluateTransitMode(now, config)

        // 4. Hıza göre mesafe eşiği
        val threshold = currentThresholdMeters(speedKmh, config)

        if (lastReevalLocation == null) {
            return reeval(now)
        }

        val distance = current.distanceTo(lastReevalLocation)
        if (distance < threshold) {
            return ReevalDecision.Skip(SkipReason.BELOW_THRESHOLD, transitModeActive)
        }

        return reeval(now)
    }

    val isTransitMode: Boolean
        @Synchronized get() = transitModeActive

    @Synchronized
    fun reset() {
        speedSamples.clear()
        lastReevalAt = 0
        transitModeActive = false
        transitModeEnterCandidateSince = null
        transitModeExitCandidateSince = null
    }

    private fun reeval(now: Long): ReevalDecision {
        lastReevalAt = now
        // Transit modunda server sync yapılmaz; sadece yerel top-N güncellenir (doc 19 §12.4)
        return ReevalDecision.Reeval(syncAllowed = !transitModeActive)
    }

    private fun computeSpeedKmh(location: Location): Double {
        val speedMs = if (location.hasSpeed() && location.speed >= 0f) location.speed.toDouble() else 0.0
        return speedMs * 3.6
    }

    private fun currentThresholdMeters(speedKmh: Double, config: AdaptiveThresholdConfig): Int {
        // tiers artan minSpeedKmh sıralı (config.clamped garantiler). Uygun en yüksek tier'ı seç.
        var threshold = config.tiers.firstOrNull()?.thresholdMeters
            ?: AdaptiveThresholdConfig.defaultTiers().first().thresholdMeters
        for (tier in config.tiers) {
            if (speedKmh >= tier.minSpeedKmh) threshold = tier.thresholdMeters else break
        }
        return threshold
    }

    private fun evaluateTransitMode(now: Long, config: AdaptiveThresholdConfig) {
        // En güncel hız örneği. Candidate-since timer "sustained for N minutes" semantiğini sağlar:
        // koşulu bozan tek bir örnek timer'ı sıfırlar (GPS jitter'a karşı dirençli).
        val currentSpeedKmh = speedSamples.peekLast()?.speedKmh ?: 0.0

        if (!transitModeActive) {
            // Giriş: sustained > entrySpeed, entryMinutes boyunca
            if (currentSpeedKmh > config.transitModeEntrySpeedKmh) {
                if (transitModeEnterCandidateSince == null) transitModeEnterCandidateSince = now
                val sustainedFor = now - (transitModeEnterCandidateSince ?: now)
                if (sustainedFor >= config.transitModeEntryMinutes * 60_000L) {
                    transitModeActive = true
                    transitModeExitCandidateSince = null
                    DengageLogger.debug("AdaptiveThreshold -> transit mode ENTER (speed sustained ${config.transitModeEntrySpeedKmh}+ km/h)")
                }
            } else {
                transitModeEnterCandidateSince = null
            }
        } else {
            // Çıkış: sustained <= exitSpeed, exitMinutes boyunca
            if (currentSpeedKmh <= config.transitModeExitSpeedKmh) {
                if (transitModeExitCandidateSince == null) transitModeExitCandidateSince = now
                val sustainedFor = now - (transitModeExitCandidateSince ?: now)
                if (sustainedFor >= config.transitModeExitMinutes * 60_000L) {
                    transitModeActive = false
                    transitModeEnterCandidateSince = null
                    DengageLogger.debug("AdaptiveThreshold -> transit mode EXIT (speed <= ${config.transitModeExitSpeedKmh} km/h)")
                }
            } else {
                transitModeExitCandidateSince = null
            }
        }
    }

    private fun purgeOldSamples(now: Long, config: AdaptiveThresholdConfig) {
        val maxWindow = maxOf(config.transitModeEntryMinutes, config.transitModeExitMinutes) * 60_000L
        val cutoff = now - maxWindow - 60_000L
        while (true) {
            val head = speedSamples.peekFirst() ?: break
            if (head.timestamp >= cutoff) break
            speedSamples.removeFirst()
        }
    }
}

sealed class ReevalDecision {
    /** [syncAllowed] = transit modunda değilsek server sync yapılabilir. */
    data class Reeval(val syncAllowed: Boolean) : ReevalDecision()
    data class Skip(val reason: SkipReason, val transitMode: Boolean) : ReevalDecision()
}

enum class SkipReason { COOLDOWN, BELOW_THRESHOLD }
