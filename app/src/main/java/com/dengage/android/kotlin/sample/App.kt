package com.dengage.android.kotlin.sample

import android.app.Application
import android.content.Context
import android.os.Looper
import com.dengage.android.kotlin.sample.push.PushNotificationReceiver
import com.dengage.android.kotlin.sample.utils.Constants
import com.dengage.geofence.DengageGeofence
import com.dengage.sdk.Dengage
import com.dengage.sdk.DengageManager
import com.dengage.sdk.data.remote.api.DeviceConfigurationPreference
import com.dengage.sdk.data.remote.api.NotificationDisplayPriorityConfiguration
import com.dengage.sdk.util.DengageLifecycleTracker
import com.dengage.sdk.util.DengageUtils
import java.util.logging.Handler

class App : Application() {
    /*lateinit var dengageManager: DengageManager
*/
    override fun onCreate() {
        super.onCreate()
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
        Dengage.init(
            context = context,
            firebaseIntegrationKey = Constants.FIREBASE_APP_INTEGRATION_KEY,
            deviceConfigurationPreference = DeviceConfigurationPreference.Huawei,
            disableOpenWebUrl = false,
            notificationDisplayPriorityConfiguration = NotificationDisplayPriorityConfiguration.SHOW_WITH_HIGH_PRIORITY

        )

        DengageGeofence.startGeofence()

      /*  Dengage.setContactKey("dasdasd")
        Dengage.setPartnerDeviceId("hi")
        Dengage.setDeviceId("22")*/
        //Dengage.setClassName("com.dengage.android.kotlin.sample.ui.activity.MainActivity2")
        Dengage.setLogStatus(true)
        Dengage.setDevelopmentStatus(true)


        Dengage.inAppLinkConfiguration("www.chaitanyamunje.com")
      /*  val filter = IntentFilter(com.dengage.sdk.util.Constants.PUSH_ACTION_CLICK_EVENT)
        registerReceiver(
            PushNotificationReceiver(),
            filter
        )*/
    }


}