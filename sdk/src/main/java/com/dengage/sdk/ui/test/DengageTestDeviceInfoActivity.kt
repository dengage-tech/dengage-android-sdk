package com.dengage.sdk.ui.test

import android.app.Activity
import android.os.Bundle
import androidx.recyclerview.widget.RecyclerView
import com.dengage.sdk.Dengage
import com.dengage.sdk.R
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.ui.test.adapter.DengageInfoAdapter
import com.dengage.sdk.util.DengageUtils

class DengageTestDeviceInfoActivity : Activity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_dengage_test_device_info)

        val infoPairs = mutableListOf<Pair<String, String?>>()
        infoPairs.add(Pair("Push Api Url", Prefs.pushApiBaseUrl))
        infoPairs.add(Pair("Event Api Url", Prefs.eventApiBaseUrl))
        infoPairs.add(Pair("Installation Id", Prefs.installationId))
        infoPairs.add(Pair("Integration Key", Dengage.getSubscription()?.integrationKey))
        infoPairs.add(Pair("Device Id", Dengage.getSubscription()?.deviceId))
        infoPairs.add(Pair("Advertising Id", Dengage.getSubscription()?.advertisingId))
        infoPairs.add(Pair("Token", Dengage.getToken()))
        infoPairs.add(Pair("Contact Key", Dengage.getSubscription()?.contactKey))
        infoPairs.add(Pair("Timezone", Dengage.getSubscription()?.timezone))
        infoPairs.add(Pair("Language", Dengage.getSubscription()?.language))
        infoPairs.add(Pair("User Permission", Dengage.getUserPermission().toString()))
        infoPairs.add(Pair("Log Visibility", Prefs.logVisibility.toString()))
        infoPairs.add(Pair("Sdk Version", DengageUtils.getSdkVersion()))

        findViewById<RecyclerView>(R.id.recyclerViewInfo).adapter = DengageInfoAdapter(infoPairs = infoPairs)
    }
}