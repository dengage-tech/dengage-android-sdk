package com.dengage.sdk.ui.test

import android.app.Activity
import android.content.Intent
import android.os.Bundle
import android.view.View
import android.widget.Button
import com.dengage.sdk.R

class DengageTestActivity : Activity(), View.OnClickListener {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_dengage_test)

        findViewById<Button>(R.id.btnPushTest).setOnClickListener(this)
        findViewById<Button>(R.id.btnInAppTest).setOnClickListener(this)
        findViewById<Button>(R.id.btnDeviceInfoTest).setOnClickListener(this)
        findViewById<Button>(R.id.btnDeviceCacheTest).setOnClickListener(this)
        findViewById<Button>(R.id.btnFetchTimeResetTest).setOnClickListener(this)
        findViewById<Button>(R.id.btnShowLogsTest).setOnClickListener(this)
    }

    override fun onClick(v: View?) {
        when (v?.id) {
            R.id.btnPushTest -> {
                startActivity(Intent(this, DengageTestPushActivity::class.java))
            }
            R.id.btnInAppTest -> {
                startActivity(Intent(this, DengageTestInAppActivity::class.java))
            }
            R.id.btnDeviceInfoTest -> {
                startActivity(Intent(this, DengageTestDeviceInfoActivity::class.java))
            }
            R.id.btnDeviceCacheTest -> {
                startActivity(Intent(this, DengageTestDeviceCacheActivity::class.java))
            }
            R.id.btnFetchTimeResetTest -> {
                startActivity(Intent(this, DengageFetchTimeResetTestActivity::class.java))
            }
            R.id.btnShowLogsTest -> {
                startActivity(Intent(this, DengageTestShowLogsActivity::class.java))
            }
        }
    }
}