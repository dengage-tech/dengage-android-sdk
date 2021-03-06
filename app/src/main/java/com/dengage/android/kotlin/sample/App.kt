package com.dengage.android.kotlin.sample

import android.app.Application
import com.dengage.android.kotlin.sample.utils.Constants
import com.dengage.sdk.Dengage
import com.dengage.sdk.util.DengageLifecycleTracker

class App : Application() {

    override fun onCreate() {
        super.onCreate()

        // to handle application bring to foreground
        registerActivityLifecycleCallbacks(DengageLifecycleTracker())

        Dengage.init(
            context = applicationContext,
            firebaseIntegrationKey = Constants.FIREBASE_APP_INTEGRATION_KEY,
            geofenceEnabled = true
        )
        Dengage.setLogStatus(true)
    }

}