package com.dengage.android.kotlin.sample

import android.app.Application
import android.content.IntentFilter
import com.dengage.android.kotlin.sample.push.PushNotificationReceiver
import com.dengage.android.kotlin.sample.utils.Constants
import com.dengage.sdk.Dengage
import com.dengage.sdk.DengageManager
import com.dengage.sdk.util.DengageLifecycleTracker

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

        Dengage.init(
            context = applicationContext,
            firebaseIntegrationKey = Constants.FIREBASE_APP_INTEGRATION_KEY
        )
        Dengage.setLogStatus(true)
        Dengage.inAppLinkConfiguration(openInAppBrowser = true, retrieveLinkOnSameScreen = true,)
      /*  val filter = IntentFilter(com.dengage.sdk.util.Constants.PUSH_ACTION_CLICK_EVENT)
        registerReceiver(
            PushNotificationReceiver(),
            filter
        )*/
    }


}