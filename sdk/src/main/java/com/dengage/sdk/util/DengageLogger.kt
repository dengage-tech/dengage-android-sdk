package com.dengage.sdk.util

import android.util.Log
import com.dengage.sdk.data.cache.Prefs

object DengageLogger {

    private const val LOGGER_TAG = "DengageSDK"
    val dengageLogs = mutableListOf<String>()

    fun debug(message: String?) {
        try {
            addToLogs(
                type = "Debug",
                message = message
            )
            if (Prefs.logVisibility && message.isNullOrEmpty().not()) {
                Log.d(LOGGER_TAG, message!!)
            }
        } catch (e: Exception) {
            e.printStackTrace()
        } catch (ex: Throwable) {
            ex.printStackTrace()

        }
    }

    fun error(message: String?) {
        try {
            addToLogs(
                type = "Error",
                message = message
            )
            if (Prefs.logVisibility && message.isNullOrEmpty().not()) {
                Log.e(LOGGER_TAG, message!!)
            }
        } catch (e: Exception) {
            e.printStackTrace()
        } catch (ex: Throwable) {
            ex.printStackTrace()

        }
    }

    fun warning(message: String?) {
        try {
            addToLogs(
                type = "Warning",
                message = message
            )
            if (Prefs.logVisibility && message.isNullOrEmpty().not()) {
                Log.w(LOGGER_TAG, message!!)
            }
        } catch (e: Exception) {
            e.printStackTrace()
        } catch (ex: Throwable) {
            ex.printStackTrace()

        }
    }

    fun info(message: String?) {
        try {
            addToLogs(
                type = "Info",
                message = message
            )
            if (Prefs.logVisibility && message.isNullOrEmpty().not()) {
                Log.i(LOGGER_TAG, message!!)
            }
        } catch (e: Exception) {
            e.printStackTrace()
        } catch (ex: Throwable) {
            ex.printStackTrace()

        }
    }

    fun verbose(message: String?) {
        try {
            addToLogs(
                type = "Verbose",
                message = message
            )
            if (Prefs.logVisibility && message.isNullOrEmpty().not()) {
                Log.v(LOGGER_TAG, message!!)
            }

        } catch (e: Exception) {
            e.printStackTrace()
        } catch (ex: Throwable) {
            ex.printStackTrace()

        }
    }

    fun addToLogs(type: String, message: String?) {
        try {
            try {
                if (message.isNullOrEmpty()) return

                if (dengageLogs.size >= 200) {
                    dengageLogs.removeAt(dengageLogs.size - 1)
                }
                dengageLogs.add(0, "$type: $message")
            } catch (e: Exception) {
                return
            }
        } catch (e: Exception) {
            e.printStackTrace()
        } catch (ex: Throwable) {
            ex.printStackTrace()

        }
    }
}