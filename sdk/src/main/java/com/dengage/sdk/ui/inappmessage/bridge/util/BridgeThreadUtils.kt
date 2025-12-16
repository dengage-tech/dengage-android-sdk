package com.dengage.sdk.ui.inappmessage.bridge.util

import android.os.Handler
import android.os.Looper

/**
 * Utility class for thread management in bridge operations
 */
object BridgeThreadUtils {

    private val mainHandler = Handler(Looper.getMainLooper())

    /**
     * Check if current thread is main thread
     * @return true if on main thread
     */
    fun isMainThread(): Boolean {
        return Looper.myLooper() == Looper.getMainLooper()
    }

    /**
     * Run action on main thread
     * @param action The action to run
     */
    fun runOnMainThread(action: () -> Unit) {
        if (isMainThread()) {
            action()
        } else {
            mainHandler.post(action)
        }
    }

    /**
     * Run action on main thread with delay
     * @param delayMillis Delay in milliseconds
     * @param action The action to run
     */
    fun runOnMainThreadDelayed(delayMillis: Long, action: () -> Unit) {
        mainHandler.postDelayed(action, delayMillis)
    }

    /**
     * Remove callbacks from main handler
     * @param action The runnable to remove
     */
    fun removeCallbacks(action: Runnable) {
        mainHandler.removeCallbacks(action)
    }
}
