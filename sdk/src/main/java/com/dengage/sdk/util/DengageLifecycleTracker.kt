package com.dengage.sdk.util

import android.app.Activity
import android.app.Application
import android.os.Bundle
import com.dengage.sdk.Dengage

class DengageLifecycleTracker : Application.ActivityLifecycleCallbacks {

    private var startedActivityCount = 0

    override fun onActivityCreated(activity: Activity, savedInstanceState: Bundle?) = Unit

    override fun onActivityStarted(activity: Activity) {
        if (startedActivityCount == 0) {
            // app went to foreground
            Dengage.getInAppMessages()
        }
        startedActivityCount++
    }

    override fun onActivityResumed(activity: Activity) = Unit

    override fun onActivityPaused(activity: Activity) = Unit

    override fun onActivityStopped(activity: Activity) {
        startedActivityCount--
    }

    override fun onActivitySaveInstanceState(activity: Activity, outState: Bundle) = Unit

    override fun onActivityDestroyed(activity: Activity) = Unit
}