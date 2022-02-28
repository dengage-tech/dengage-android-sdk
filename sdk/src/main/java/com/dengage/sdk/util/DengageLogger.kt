package com.dengage.sdk.util

import android.util.Log
import com.dengage.sdk.data.cache.Prefs

object DengageLogger {

    private const val LOGGER_TAG = "DengageSDK"
    val dengageLogs = mutableListOf<String>()

    fun debug(message: String?) {
        addToLogs(
            type = "Debug",
            message = message
        )
        if (Prefs.logVisibility && message.isNullOrEmpty().not()) {
            Log.d(LOGGER_TAG, message!!)
        }
    }

    fun error(message: String?) {
        addToLogs(
            type = "Error",
            message = message
        )
        if (Prefs.logVisibility && message.isNullOrEmpty().not()) {
            Log.e(LOGGER_TAG, message!!)
        }
    }

    fun warning(message: String?) {
        addToLogs(
            type = "Warning",
            message = message
        )
        if (Prefs.logVisibility && message.isNullOrEmpty().not()) {
            Log.w(LOGGER_TAG, message!!)
        }
    }

    fun info(message: String?) {
        addToLogs(
            type = "Info",
            message = message
        )
        if (Prefs.logVisibility && message.isNullOrEmpty().not()) {
            Log.i(LOGGER_TAG, message!!)
        }
    }

    fun verbose(message: String?) {
        addToLogs(
            type = "Verbose",
            message = message
        )
        if (Prefs.logVisibility && message.isNullOrEmpty().not()) {
            Log.v(LOGGER_TAG, message!!)
        }
    }

    fun addToLogs(type: String, message: String?) {
        if (message.isNullOrEmpty()) return

        if (dengageLogs.size >= 200) {
            dengageLogs.removeLast()
        }
        dengageLogs.add(0, "$type: $message")
    }
}