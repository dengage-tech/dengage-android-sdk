package com.dengage.sdk.ui.test

import android.app.Activity
import android.os.Bundle
import androidx.recyclerview.widget.RecyclerView
import com.dengage.sdk.R
import com.dengage.sdk.util.EdgeToEdgeUtils
import com.dengage.sdk.ui.test.adapter.DengageInfoAdapter
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.extension.toJson

class DengageTestShowLogsActivity : Activity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        
        // Enable edge-to-edge display for Android 15
        EdgeToEdgeUtils.enableEdgeToEdge(this)
        
        setContentView(R.layout.activity_dengage_test_show_logs)

        val infoPairs = mutableListOf<Pair<String, String?>>()
        infoPairs.add(Pair("Dengage Logs", DengageLogger.dengageLogs.toJson()))

        findViewById<RecyclerView>(R.id.recyclerViewLogs).adapter = DengageInfoAdapter(infoPairs = infoPairs)
    }
}