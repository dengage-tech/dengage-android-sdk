package com.dengage.geofenceengine.worker

import android.content.Context
import androidx.work.Worker
import androidx.work.WorkerParameters
import com.dengage.geofenceengine.DengageGeofenceEngine
import com.dengage.sdk.util.DengageLogger

/**
 * Wake-up cap pause süresi dolduğunda SLC/FLP resume tetikler (doc 21 §11 K13).
 * Çoklu fırsatçı kanaldan biri (bu planlanmış worker dahil) [DengageGeofenceEngine.attemptResume]'u çağırır.
 */
class ResumeSlcWorker(context: Context, params: WorkerParameters) : Worker(context, params) {

    override fun doWork(): Result {
        return try {
            DengageGeofenceEngine.getInstance(applicationContext).attemptResume()
            Result.success()
        } catch (e: Exception) {
            DengageLogger.error("ResumeSlcWorker -> ${e.message}")
            Result.success()
        }
    }
}
