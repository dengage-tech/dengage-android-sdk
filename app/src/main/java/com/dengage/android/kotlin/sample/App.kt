package com.dengage.android.kotlin.sample

import android.app.Application
import android.app.NotificationChannel
import android.app.NotificationManager
import android.content.Context
import android.os.Build
import com.dengage.android.kotlin.sample.utils.Constants
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.remote.api.DeviceConfigurationPreference
import com.dengage.sdk.data.remote.api.NotificationDisplayPriorityConfiguration
import com.dengage.sdk.liveupdate.DengageLiveUpdateManager
import com.dengage.android.kotlin.sample.liveupdate.DeliveryLiveUpdateHandler
import com.dengage.android.kotlin.sample.liveupdate.SportsLiveUpdateHandler
import com.dengage.sdk.util.DengageLifecycleTracker
import com.dengage.hms.DengageHmsManager
import com.dengage.geofence.DengageGeofence
import com.dengage.geofence.GeofenceInterceptor
import com.dengage.sdk.data.remote.api.ApiUrlConfiguration
import android.util.Log
import androidx.core.app.NotificationCompat

class App : Application() {

    companion object {
        private const val GEOFENCE_CHANNEL_ID = "geofence_intercept_channel"
        private const val GEOFENCE_CHANNEL_NAME = "Geofence Intercept"
        private val notificationIdGenerator = java.util.concurrent.atomic.AtomicInteger(1000)
    }

    /*lateinit var dengageManager: DengageManager
*/
    override fun onCreate() {
        super.onCreate()
        createGeofenceNotificationChannel()
        /*  val data = HashMap<String, Any>()
          data["page_type"] = "test123"
          Dengage.pageView(data,this)*/
        // to handle application bring to foreground
        registerActivityLifecycleCallbacks(DengageLifecycleTracker())

        /* dengageManager = DengageManager
             .getInstance(applicationContext)
             .setLogStatus(true)
             .setFirebaseIntegrationKey(Constants.FIREBASE_APP_INTEGRATION_KEY)
             .init()*/
        val context :Context  = this

        val dengageHmsManager = DengageHmsManager()


        val apiUrlConfiguration = ApiUrlConfiguration(
            denEventApiUrl = "https://dev-push.dengage.com",
            denPushApiUrl = "https://dev-push.dengage.com",
            denInAppApiUrl = "https://dev-push.dengage.com",
            denGeofenceApiUrl = "https://dev-push.dengage.com/geoapi/",
            fetchRealTimeInAppApiUrl = "https://dev-inapp.lib.dengage.com/"
        )

        Dengage.init(
            context = context,
            firebaseIntegrationKey = Constants.FIREBASE_APP_INTEGRATION_KEY,
            dengageHmsManager = dengageHmsManager,
            deviceConfigurationPreference = DeviceConfigurationPreference.Google,
            disableOpenWebUrl = false,
            notificationDisplayPriorityConfiguration = NotificationDisplayPriorityConfiguration.SHOW_WITH_HIGH_PRIORITY
        )

        /*  Dengage.setContactKey("dasdasd")
          Dengage.setPartnerDeviceId("hi")
          Dengage.setDeviceId("22")*/
        //Dengage.setClassName("com.dengage.android.kotlin.sample.ui.activity.MainActivity2")
        Dengage.setLogStatus(true)
        Dengage.setDevelopmentStatus(true)


        ////Dengage.inAppLinkConfiguration("www.chaitanyamunje.com")

        DengageGeofence.geofenceInterceptor = object : GeofenceInterceptor {
            override fun onGeofenceEnter(
                latitude: Double,
                longitude: Double,
                radius: Double,
                clusterId: Int,
                clusterName: String?,
                geofenceItemId: Int,
                geofenceItemName: String?
            ) {
                Log.d(
                    "GeofenceInterceptor",
                    "enter | lat=$latitude, lon=$longitude, radius=$radius, clusterId=$clusterId, clusterName=$clusterName, itemId=$geofenceItemId, itemName=$geofenceItemName"
                )
                showGeofenceEnterNotification(
                    latitude = latitude,
                    longitude = longitude,
                    radius = radius,
                    clusterId = clusterId,
                    clusterName = clusterName,
                    geofenceItemId = geofenceItemId,
                    geofenceItemName = geofenceItemName
                )
            }
        }

        DengageGeofence.startGeofence()

        // Register Live Update handlers
        DengageLiveUpdateManager.register("ExampleAppFirstWidgetAttributes", DeliveryLiveUpdateHandler())
        DengageLiveUpdateManager.register("ExampleAppSecondWidgetAttributes", SportsLiveUpdateHandler())


        /*  val filter = IntentFilter(com.dengage.sdk.util.Constants.PUSH_ACTION_CLICK_EVENT)
          registerReceiver(
              PushNotificationReceiver(),
              filter
          )*/
    }

    private fun createGeofenceNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            val manager = getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
            if (manager.getNotificationChannel(GEOFENCE_CHANNEL_ID) == null) {
                val channel = NotificationChannel(
                    GEOFENCE_CHANNEL_ID,
                    GEOFENCE_CHANNEL_NAME,
                    NotificationManager.IMPORTANCE_HIGH
                )
                manager.createNotificationChannel(channel)
            }
        }
    }

    private fun showGeofenceEnterNotification(
        latitude: Double,
        longitude: Double,
        radius: Double,
        clusterId: Int,
        clusterName: String?,
        geofenceItemId: Int,
        geofenceItemName: String?
    ) {
        try {
            val title = if (geofenceItemName != null) "Geofence Enter: $geofenceItemName" else "Geofence Enter"
            val notification = NotificationCompat.Builder(this, GEOFENCE_CHANNEL_ID)
                .setSmallIcon(android.R.drawable.ic_dialog_info)
                .setContentTitle(title)
                .setContentText("cluster=${clusterName ?: clusterId} item=${geofenceItemName ?: geofenceItemId}")
                .setStyle(
                    NotificationCompat.BigTextStyle().bigText(
                        "clusterId=$clusterId\nclusterName=$clusterName\nitemId=$geofenceItemId\nitemName=$geofenceItemName\nlat=$latitude\nlon=$longitude\nradius=$radius"
                    )
                )
                .setPriority(NotificationCompat.PRIORITY_HIGH)
                .setAutoCancel(true)
                .build()

            val manager = getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
            manager.notify(notificationIdGenerator.incrementAndGet(), notification)
        } catch (e: Exception) {
            Log.e("GeofenceInterceptor", "Failed to show notification", e)
        }
    }
}