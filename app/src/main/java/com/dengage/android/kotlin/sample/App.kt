package com.dengage.android.kotlin.sample

import android.app.Application
import android.content.Context
import com.dengage.android.kotlin.sample.utils.Constants
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.remote.api.DeviceConfigurationPreference
import com.dengage.sdk.data.remote.api.NotificationDisplayPriorityConfiguration
import com.dengage.sdk.util.DengageLifecycleTracker
import com.dengage.hms.DengageHmsManager
import com.dengage.geofence.DengageGeofence
import com.dengage.sdk.data.remote.api.ApiUrlConfiguration
import com.google.firebase.FirebaseApp

class App : Application() {
    /*lateinit var dengageManager: DengageManager
*/
    override fun onCreate() {
        super.onCreate()
        FirebaseApp.initializeApp(this)
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


        Dengage.inAppLinkConfiguration("www.chaitanyamunje.com")

        DengageGeofence.startGeofence()


      /*  val filter = IntentFilter(com.dengage.sdk.util.Constants.PUSH_ACTION_CLICK_EVENT)
        registerReceiver(
            PushNotificationReceiver(),
            filter
        )*/
    }


}