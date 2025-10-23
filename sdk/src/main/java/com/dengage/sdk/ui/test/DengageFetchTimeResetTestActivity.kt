package com.dengage.sdk.ui.test

import android.app.Activity
import android.os.Bundle
import android.view.View
import android.widget.Button
import android.widget.TextView
import com.dengage.sdk.R
import com.dengage.sdk.util.EdgeToEdgeUtils
import com.dengage.sdk.data.cache.Prefs

class DengageFetchTimeResetTestActivity : Activity(), View.OnClickListener {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        
        // Enable edge-to-edge display for Android 15
        EdgeToEdgeUtils.enableEdgeToEdge(this)
        
        setContentView(R.layout.activity_dengage_fetch_time_reset_test)

        findViewById<Button>(R.id.btnSdkParametersFetchTimeReset).setOnClickListener(this)
        findViewById<Button>(R.id.btnAppTrackingTimeReset).setOnClickListener(this)
        findViewById<Button>(R.id.btnInAppFetchTimeReset).setOnClickListener(this)
        findViewById<Button>(R.id.btnInAppShowTimeReset).setOnClickListener(this)
        findViewById<Button>(R.id.btnInboxFetchTimeReset).setOnClickListener(this)

        setSdkParametersFetchTimeMessage()
        setAppTrackingTimeMessage()
        setInAppFetchTimeMessage()
        setInAppShowTimeMessage()
        setInboxFetchTimeMessage()
    }

    private fun setSdkParametersFetchTimeMessage() {
        findViewById<TextView>(R.id.textViewSdkParametersFetchTimeMessage).text =
            Prefs.sdkParameters?.lastFetchTimeInMillis?.toString()
    }

    private fun setAppTrackingTimeMessage() {
        findViewById<TextView>(R.id.textViewAppTrackingTimeMessage).text =
            Prefs.appTrackingTime.toString()
    }

    private fun setInAppFetchTimeMessage() {
        findViewById<TextView>(R.id.textViewInAppFetchTimeMessage).text =
            Prefs.inAppMessageFetchTime.toString()
    }

    private fun setInAppShowTimeMessage() {
        findViewById<TextView>(R.id.textViewInAppShowTimeMessage).text =
            Prefs.inAppMessageShowTime.toString()
    }

    private fun setInboxFetchTimeMessage() {
        findViewById<TextView>(R.id.textViewInboxFetchTimeMessage).text =
            Prefs.inboxMessageFetchTime.toString()
    }

    override fun onClick(v: View?) {
        when (v?.id) {
            R.id.btnSdkParametersFetchTimeReset -> {
                val sdkParameters = Prefs.sdkParameters
                sdkParameters?.let {
                    it.lastFetchTimeInMillis = 0L
                    Prefs.sdkParameters = it
                }
                setSdkParametersFetchTimeMessage()
            }
            R.id.btnAppTrackingTimeReset -> {
                Prefs.appTrackingTime = 0L
                setAppTrackingTimeMessage()
            }
            R.id.btnInAppFetchTimeReset -> {
                Prefs.inAppMessageFetchTime = 0L
                setInAppFetchTimeMessage()
            }
            R.id.btnInAppShowTimeReset -> {
                Prefs.inAppMessageShowTime = 0L
                setInAppShowTimeMessage()
            }
            R.id.btnInboxFetchTimeReset -> {
                Prefs.inboxMessageFetchTime = 0L
                setInboxFetchTimeMessage()
            }
        }
    }
}