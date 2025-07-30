package com.dengage.sdk.push

import android.Manifest
import android.os.Build
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.result.contract.ActivityResultContracts
import androidx.annotation.RequiresApi
import androidx.core.app.NotificationManagerCompat

interface NotificationPermissionCallback {
    fun onPermissionResult(granted: Boolean)
}

class NotificationPermissionActivity : ComponentActivity() {

    companion object {
        var callback: NotificationPermissionCallback? = null
    }

    private val requestPermissionLauncher = registerForActivityResult(
        ActivityResultContracts.RequestPermission()
    ) { enabled ->
        callback?.onPermissionResult(enabled)
        finish()
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        requestNotificationPermission()
    }


    private fun requestNotificationPermission() {
        val enabled = NotificationManagerCompat.from(this).areNotificationsEnabled()
        if (!enabled) {
            requestPermissionLauncher.launch(Manifest.permission.POST_NOTIFICATIONS)
        } else {
            callback?.onPermissionResult(enabled)
            finish()
        }
    }
}
