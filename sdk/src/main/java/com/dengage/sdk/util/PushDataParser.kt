package com.dengage.sdk.util

import android.content.Intent
import android.text.TextUtils
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.push.PushMessageCallback

object PushDataParser {

    fun parseIntent(intent: Intent, pushMessageCallback: PushMessageCallback) {
        try {
            if (intent != null) {
                if (intent.action != null) {

                    var uri: String? = null
                    if (intent.extras != null) {
                        var message = Message.createFromIntent(intent.extras!!)
                        val rawJson = intent.extras!!.getString("RAW_DATA")
                        if (!TextUtils.isEmpty(rawJson)) {
                            message = GsonHolder.gson.fromJson(rawJson, Message::class.java)
                        }
                        pushMessageCallback.dataFetched(message)
                        if (intent.action == Constants.PUSH_ACTION_CLICK_EVENT) {
                            val id = intent.extras!!.getString("id", "")
                            pushMessageCallback.onActionClick(intent, message, id)
                        }

                    }


                }

            }
        } catch (e: Exception) {
        }
    }
}