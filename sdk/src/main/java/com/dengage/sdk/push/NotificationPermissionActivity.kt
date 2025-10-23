package com.dengage.sdk.push

import android.Manifest
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.result.contract.ActivityResultContracts
import androidx.core.app.NotificationManagerCompat
import com.dengage.sdk.util.EdgeToEdgeUtils

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
        
        // Enable edge-to-edge display for Android 15 with proper insets handling
        EdgeToEdgeUtils.enableEdgeToEdgeWithInsets(this)
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
