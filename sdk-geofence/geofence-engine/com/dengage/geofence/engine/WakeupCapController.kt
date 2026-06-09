package com.dengage.geofence.engine

import com.dengage.sdk.domain.configuration.model.WakeupCapConfig
import com.dengage.sdk.util.DengageLogger
import java.util.ArrayDeque

/**
 * Wake-up cap + SLC/FLP pause/resume (K13, doc 21 §11).
 * OS'un periyodik konum izin dialog'unu tetiklememek için saatlik wake-up sayısına hard cap uygular.
 * Cap aşılınca location updates pause edilir; çoklu fırsatçı kanaldan biri [attemptResume] çağırır.
 *
 * FLP/WorkManager bağımlılıkları lambda olarak enjekte edilir (decoupled, test edilebilir).
 */
class WakeupCapController(
    private val configProvider: () -> WakeupCapConfig,
    private val onPause: () -> Unit,
    private val onResume: () -> Unit,
    private val scheduleResume: (delayMinutes: Long) -> Unit
) {

    private val timestamps = ArrayDeque<Long>()

    @Volatile
    private var paused: Boolean = false
    private var pausedAt: Long = 0

    val isPaused: Boolean get() = paused

    /** Her SLC/FLP wake-up'ta çağrılır. Cap aşıldıysa pause başlatır. */
    @Synchronized
    fun recordWakeup(now: Long = System.currentTimeMillis()): WakeupAction {
        val config = configProvider()
        purgeOldTimestamps(now, config)

        if (paused) return WakeupAction.SKIP_PAUSED

        timestamps.addLast(now)
        if (timestamps.size > config.hourlyMax) {
            pause(now, config)
            return WakeupAction.PAUSE_AND_SKIP
        }
        return WakeupAction.PROCEED
    }

    /** BGAppRefresh/Worker, push delivery, app foreground veya geofence callback'ten çağrılır. */
    @Synchronized
    fun attemptResume(now: Long = System.currentTimeMillis()) {
        if (!paused) return
        val config = configProvider()
        val pauseElapsed = now - pausedAt
        if (pauseElapsed < config.pauseMinutes * 60_000L) {
            DengageLogger.debug("WakeupCap -> resume attempt too early (${pauseElapsed / 1000}s elapsed)")
            return
        }
        paused = false
        timestamps.clear()
        DengageLogger.debug("WakeupCap -> resume")
        onResume()
    }

    @Synchronized
    fun reset() {
        paused = false
        timestamps.clear()
        pausedAt = 0
    }

    private fun pause(now: Long, config: WakeupCapConfig) {
        paused = true
        pausedAt = now
        DengageLogger.debug("WakeupCap -> cap exceeded (${config.hourlyMax}/${config.slidingWindowMinutes}min), pausing for ${config.pauseMinutes}min")
        onPause()
        scheduleResume(config.pauseMinutes.toLong())
    }

    private fun purgeOldTimestamps(now: Long, config: WakeupCapConfig) {
        val cutoff = now - config.slidingWindowMinutes * 60_000L
        while (true) {
            val head = timestamps.peekFirst() ?: break
            if (head >= cutoff) break
            timestamps.removeFirst()
        }
    }
}

enum class WakeupAction { PROCEED, SKIP_PAUSED, PAUSE_AND_SKIP }
