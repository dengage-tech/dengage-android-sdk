package com.dengage.sdk.liveupdate

import android.content.Context
import com.dengage.sdk.util.DengageLogger
import java.util.concurrent.ConcurrentHashMap

/**
 * SDK-level manager for Android Live Update notifications.
 *
 * This class provides the **framework** for live updates.
 * It does NOT contain any notification templates — the host application
 * registers its own [LiveUpdateHandler] implementations that are responsible
 * for building and posting notifications.
 *
 * ### Architecture
 * ```
 * FCM Push → FcmMessagingService → DengageLiveUpdateManager.handleFromFcmData()
 *                                         ↓
 *                                  parse "live_notification" JSON → LiveUpdatePayload
 *                                         ↓
 *                                  handler registry lookup by activity_type
 *                                         ↓
 *                                  LiveUpdateHandler.onUpdate(context, payload)
 *                                         ↓
 *                                  App builds & posts its own notification
 * ```
 *
 * ### Usage
 * ```kotlin
 * // 1. Create a handler (in your app)
 * class MyDeliveryHandler : LiveUpdateHandler { ... }
 *
 * // 2. Register at app startup (Application.onCreate)
 * DengageLiveUpdateManager.register("delivery", MyDeliveryHandler())
 *
 * // 3. FCM messages with live_notification containing activity_type="delivery"
 * //    are automatically routed
 * ```
 *
 * ### Expected FCM data payload
 * ```json
 * {
 *   "live_notification": "{\"activity_type\":\"delivery\",\"event\":\"update\",\"activityId\":\"uuid\",\"content_state\":{...}}"
 * }
 * ```
 */
object DengageLiveUpdateManager {

    private val handlers = ConcurrentHashMap<String, LiveUpdateHandler>()

    // -------------------------------------------------------------------------
    // Public API
    // -------------------------------------------------------------------------

    /**
     * Register a [LiveUpdateHandler] for the given activity type.
     * Replaces any previously registered handler for the same type.
     */
    fun register(type: String, handler: LiveUpdateHandler) {
        handlers[type] = handler
        DengageLogger.verbose("LiveUpdate handler registered for type: $type")
    }

    /**
     * Remove the handler for the given type.
     */
    fun unregister(type: String) {
        handlers.remove(type)
        DengageLogger.verbose("LiveUpdate handler unregistered for type: $type")
    }

    /**
     * Returns `true` if a handler is registered for the given type.
     */
    fun isRegistered(type: String): Boolean = handlers.containsKey(type)

    // -------------------------------------------------------------------------
    // FCM routing (called by FcmMessagingService)
    // -------------------------------------------------------------------------

    /**
     * Entry point for FCM messages that carry a `live_notification` key.
     * Parses the JSON, resolves the handler by activity_type, and dispatches.
     */
    internal fun handleFromFcmData(context: Context, data: Map<String, String>) {
        val json = data["live_notification"]
        if (json.isNullOrEmpty()) {
            DengageLogger.verbose("LiveUpdate ignored — empty live_notification")
            return
        }

        val payload = LiveUpdatePayload.fromJson(json)
        if (payload == null) {
            DengageLogger.error("LiveUpdate ignored — failed to parse live_notification")
            return
        }

        val handler = handlers[payload.activityType]
        if (handler != null) {
            DengageLogger.verbose("LiveUpdate ${payload.event} → ${payload.activityType} [${payload.activityId}]")
            handler.onUpdate(context, payload)
        } else {
            DengageLogger.verbose("LiveUpdate ignored — no handler for type: ${payload.activityType}")
        }
    }
}
