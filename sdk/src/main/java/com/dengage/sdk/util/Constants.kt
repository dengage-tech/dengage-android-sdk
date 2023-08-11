package com.dengage.sdk.util

object Constants {

    const val PREFS_NAME = "___DEN_DEVICE_UNIQUE_ID___"
    const val DATE_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"
    const val PUSH_API_URI = "https://push.dengage.com"
    const val EVENT_API_URI = "https://event.dengage.com"
    const val MESSAGE_SOURCE = "DENGAGE"

    const val PUSH_ITEM_CLICK_EVENT = "com.dengage.push.intent.ITEM_CLICK"
    const val PUSH_RECEIVE_EVENT = "com.dengage.push.intent.RECEIVE"
    const val PUSH_OPEN_EVENT = "com.dengage.push.intent.OPEN"
    const val PUSH_DELETE_EVENT = "com.dengage.push.intent.DELETE"
    const val PUSH_ACTION_CLICK_EVENT = "com.dengage.push.intent.ACTION_CLICK"
    const val NOTIFICATION_CHANNEL_ID = "3374143"
    const val NOTIFICATION_CHANNEL_NAME = "General"

    internal var deviceId=""
    var deviceToken=""

   var isActivityPerformed =false

    var isBCRegistered =false
}

