package com.dengage.geofence.engine

import android.content.Context
import androidx.work.ExistingWorkPolicy
import androidx.work.OneTimeWorkRequestBuilder
import androidx.work.WorkManager
import com.dengage.geofence.engine.storage.model.Fence
import com.dengage.geofence.engine.worker.ActiveWindowWorker
import com.dengage.sdk.util.DengageLogger
import java.util.concurrent.TimeUnit

/**
 * `next_state_change_at` için boundary scheduling (doc 21 §6.5).
 * Kayıtlı fence'lerin en yakın gelecekteki aktiflik durum değişimine WorkManager OneTimeWork planlar;
 * boundary'de top-N reeval tetiklenir (best-effort).
 */
class ActiveWindowScheduler(private val context: Context) {

    fun schedule(fences: List<Fence>, now: Long = System.currentTimeMillis()) {
        val nextBoundary = fences
            .mapNotNull { it.nextStateChangeAtMillis }
            .filter { it > now }
            .minOrNull()

        if (nextBoundary == null) {
            cancel()
            return
        }

        val delay = (nextBoundary - now).coerceAtLeast(MIN_DELAY_MS)
        val work = OneTimeWorkRequestBuilder<ActiveWindowWorker>()
            .setInitialDelay(delay, TimeUnit.MILLISECONDS)
            .addTag(WORK_NAME)
            .build()
        WorkManager.getInstance(context).enqueueUniqueWork(
            WORK_NAME, ExistingWorkPolicy.REPLACE, work
        )
        DengageLogger.debug("ActiveWindowScheduler -> scheduled reeval in ${delay / 1000}s")
    }

    fun cancel() {
        WorkManager.getInstance(context).cancelUniqueWork(WORK_NAME)
    }

    companion object {
        const val WORK_NAME = "dengage_geofence_active_window"
        private const val MIN_DELAY_MS = 60_000L
    }
}
