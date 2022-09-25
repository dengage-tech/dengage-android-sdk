package com.dengage.android.kotlin.sample

import android.app.Application
import com.dengage.android.kotlin.sample.utils.Constants
import com.dengage.sdk.Dengage
import com.dengage.sdk.DengageManager
import com.dengage.sdk.util.DengageLifecycleTracker

class App : Application() {
    lateinit var dengageManager: DengageManager

    override fun onCreate() {
        super.onCreate()

        // to handle application bring to foreground
        registerActivityLifecycleCallbacks(DengageLifecycleTracker())

        dengageManager = DengageManager
            .getInstance(applicationContext)
            .setLogStatus(true)
            .setFirebaseIntegrationKey(Constants.FIREBASE_APP_INTEGRATION_KEY)
            .init()
    }

}