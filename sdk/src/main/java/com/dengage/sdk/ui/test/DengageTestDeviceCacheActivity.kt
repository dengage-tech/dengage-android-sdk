package com.dengage.sdk.ui.test

import android.app.Activity
import android.os.Bundle
import androidx.recyclerview.widget.RecyclerView
import com.dengage.sdk.Dengage
import com.dengage.sdk.R
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.ui.test.adapter.DengageInfoAdapter
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.extension.toJson

class DengageTestDeviceCacheActivity : Activity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_dengage_test_device_info)

        val infoPairs = mutableListOf<Pair<String, String?>>()
        infoPairs.add(Pair("Sdk Parameters", Prefs.sdkParameters?.toJson()))
        infoPairs.add(Pair("Subscription", Prefs.subscription.toJson()))
        infoPairs.add(Pair("In App Messages", Prefs.inAppMessages?.toJson()))
        infoPairs.add(Pair("App Tracking Time", Prefs.appTrackingTime.toString()))
        infoPairs.add(Pair("In App Fetch Time", Prefs.inAppMessageFetchTime.toString()))
        infoPairs.add(Pair("In App Show Time", Prefs.inAppMessageShowTime.toString()))
        infoPairs.add(Pair("Inbox Message Fetch Time", Prefs.inboxMessageFetchTime.toString()))
        infoPairs.add(Pair("App Session Time", Prefs.appSessionTime.toString()))
        infoPairs.add(Pair("App Session Id", Prefs.appSessionId))

        findViewById<RecyclerView>(R.id.recyclerViewInfo).adapter = DengageInfoAdapter(infoPairs = infoPairs)
    }
}