package com.dengage.sdk.push

import android.content.Intent
import com.dengage.sdk.domain.push.model.Message

interface PushMessageCallback {

    fun dataFetched(message: Message)
    fun onActionClick( intent: Intent,message: Message,clickedId:String)

}