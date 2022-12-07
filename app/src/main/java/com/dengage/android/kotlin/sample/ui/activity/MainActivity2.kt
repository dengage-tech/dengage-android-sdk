package com.dengage.android.kotlin.sample.ui.activity

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import com.dengage.android.kotlin.sample.R
import com.dengage.sdk.Dengage

class MainActivity2 : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main2)

        Log.d("oops", intent.toString())

        Dengage.setNavigation(this)
    }
}