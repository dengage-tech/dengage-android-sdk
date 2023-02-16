package com.dengage.android.kotlin.sample.ui.activity

import android.content.Intent
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import androidx.navigation.findNavController
import com.dengage.android.kotlin.sample.R
import com.dengage.sdk.Dengage
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.push.PushMessageCallback
import com.dengage.sdk.util.GsonHolder
import com.dengage.sdk.util.PushDataParser

class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        PushDataParser.parseIntent(intent, pushMessageCallback = object : PushMessageCallback {
            override fun dataFetched(message: Message) {
                Log.d("logs",GsonHolder.toJson(message))
            }

            override fun onActionClick(intent: Intent, message: Message, clickedId: String) {
                Log.d("logs","dsd " +
                        ""+GsonHolder.toJson(message)+" sdsd "+clickedId)
            }


        })
    }

    override fun onActivityResult(requestCode: Int, resultCode: Int, data: Intent?) {
        super.onActivityResult(requestCode, resultCode, data)
        Log.d("oops","sdsd $data")
    }


    override fun onSupportNavigateUp(): Boolean =
        findNavController(R.id.navigationHostFragment).navigateUp()

}