package com.dengage.geofence.engine.worker

import android.content.Context
import androidx.work.Worker
import androidx.work.WorkerParameters
import com.dengage.geofence.engine.DengageGeofenceEngine
import com.dengage.sdk.util.DengageLogger

/**
 * `next_state_change_at` boundary'sinde top-N reeval tetikler (doc 21 §6.5 ActiveWindowScheduler).
 */
class ActiveWindowWorker(context: Context, params: WorkerParameters) : Worker(context, params) {

    override fun doWork(): Result {
        return try {
            DengageGeofenceEngine.getInstance(applicationContext).onActiveWindowBoundary()
            Result.success()
        } catch (e: Exception) {
            DengageLogger.error("ActiveWindowWorker -> ${e.message}")
            Result.success()
        }
    }
}
