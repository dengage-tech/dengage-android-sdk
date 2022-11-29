package com.dengage.sdk.manager.geofence

import android.app.job.JobInfo
import android.app.job.JobParameters
import android.app.job.JobScheduler
import android.app.job.JobService
import android.content.ComponentName
import android.content.Context
import android.location.Location
import android.os.Build
import android.os.Handler
import android.os.Looper
import android.os.PersistableBundle
import androidx.annotation.RequiresApi
import java.util.concurrent.atomic.AtomicInteger
import com.dengage.sdk.Dengage
import com.google.android.gms.location.Geofence
import com.google.android.gms.location.GeofencingEvent
import com.dengage.sdk.domain.geofence.model.GeofenceLocationSource as GLC
import com.dengage.sdk.util.DengageLogger as DL


@RequiresApi(Build.VERSION_CODES.LOLLIPOP)
class GeofenceJobScheduler : JobService() {

    internal companion object {
        private const val EXTRA_LATITUDE = "latitude"
        private const val EXTRA_LONGITUDE = "longitude"
        private const val EXTRA_ACCURACY = "accuracy"
        private const val EXTRA_PROVIDER = "provider"
        private const val EXTRA_TIME = "time"
        private const val EXTRA_SOURCE = "source"
        private const val EXTRA_GEOFENCE_REQUEST_ID = "geofence_request_id"

        private const val BASE_JOB_ID = 19200423

        private val counter = AtomicInteger()

        internal fun scheduleJob(context: Context, location: Location, source: GLC, geofenceRequestId: String?) {
            if (!Dengage.initialized) Dengage.init(context)
            val componentName = ComponentName(context, GeofenceJobScheduler::class.java)
            val extras = PersistableBundle().apply {
                putDouble(EXTRA_LATITUDE, location.latitude)
                putDouble(EXTRA_LONGITUDE, location.longitude)
                putDouble(EXTRA_ACCURACY, location.accuracy.toDouble())
                putString(EXTRA_PROVIDER, location.provider)
                putLong(EXTRA_TIME, location.time)
                putString(EXTRA_SOURCE, source.name)
                putString(EXTRA_GEOFENCE_REQUEST_ID, geofenceRequestId)
            }

            val jobId = BASE_JOB_ID + (counter.incrementAndGet() % 1)

            val jobInfo = JobInfo.Builder(jobId, componentName)
                .setExtras(extras)
                .setMinimumLatency(0)
                .setOverrideDeadline(0)
                .setRequiredNetworkType(JobInfo.NETWORK_TYPE_NONE)
                .build()

            val jobScheduler = context.getSystemService(JOB_SCHEDULER_SERVICE) as JobScheduler
            val result = jobScheduler.schedule(jobInfo)
            if (result == JobScheduler.RESULT_SUCCESS) {
                DL.debug("Scheduling location job | location = $location; source = ${source.getStringValue()};")
            } else {
                DL.debug("Failed to schedule location job | location = $location; source = ${source.getStringValue()};")
            }
        }
    }

    override fun onStartJob(params: JobParameters): Boolean {
        if (!Dengage.initialized) {
            Dengage.init(this.applicationContext)
        }

        val extras = params.extras
        val latitude = extras.getDouble(EXTRA_LATITUDE)
        val longitude = extras.getDouble(EXTRA_LONGITUDE)
        val accuracy = extras.getDouble(EXTRA_ACCURACY).toFloat()
        val provider = extras.getString(EXTRA_PROVIDER)
        val time = extras.getLong(EXTRA_TIME)

        val location = Location(provider).apply {
            this.latitude = latitude
            this.longitude = longitude
            this.accuracy = accuracy
            this.time = time
        }

        val sourceStr = extras.getString(EXTRA_SOURCE) ?: return false
        val source = GLC.valueOf(sourceStr)

        val geofenceRequestId = extras.getString(EXTRA_GEOFENCE_REQUEST_ID)

        Dengage.handleLocation(this.applicationContext, location, source, geofenceRequestId)

        Handler(Looper.getMainLooper()).post({
            this.jobFinished(params, false)
        })

        counter.set(0)
        return true
    }

    override fun onStopJob(params: JobParameters): Boolean {
        if (!Dengage.initialized) {
            Dengage.init(this.applicationContext)
        }
        return false
    }

}