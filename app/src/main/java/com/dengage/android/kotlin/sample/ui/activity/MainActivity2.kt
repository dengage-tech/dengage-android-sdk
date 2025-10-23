package com.dengage.android.kotlin.sample.ui.activity

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import com.dengage.android.kotlin.sample.R
import com.dengage.sdk.Dengage
import com.dengage.sdk.util.EdgeToEdgeUtils

class MainActivity2 : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        
        setContentView(R.layout.activity_main2)
        
        // Enable edge-to-edge display for Android 15 with proper insets handling
        EdgeToEdgeUtils.enableEdgeToEdgeWithInsets(this)

        Log.d("oops", intent.toString())

        Dengage.setCurrentActivity(this)
        Dengage.restartApplicationAfterPushClick(true)
    }
}