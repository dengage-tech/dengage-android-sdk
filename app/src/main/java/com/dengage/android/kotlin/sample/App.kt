package com.dengage.android.kotlin.sample

import android.app.Application
import com.dengage.android.kotlin.sample.utils.Constants
import com.dengage.sdk.Dengage
import com.dengage.sdk.util.DengageLifecycleTracker

class App : Application() {

    override fun onCreate() {
        super.onCreate()
/*          val data = HashMap<String, Any>()
              data["page_type"] = "test123"
              Dengage.sendDeviceEvent("rh",data)*/
        // to handle application bring to foreground
        registerActivityLifecycleCallbacks(DengageLifecycleTracker())

        Dengage.init(
            context = applicationContext,
            firebaseIntegrationKey = Constants.FIREBASE_APP_INTEGRATION_KEY,
        )
        Dengage.setLogStatus(true)
        Dengage.inAppLinkConfiguration("www.chaitanyamunje.com")
        /*  val filter = IntentFilter(com.dengage.sdk.util.Constants.PUSH_ACTION_CLICK_EVENT)
      registerReceiver(
          PushNotificationReceiver(),
          filter
      )*/
    }

}