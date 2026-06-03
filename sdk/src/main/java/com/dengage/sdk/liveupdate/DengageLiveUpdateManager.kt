package com.dengage.sdk.liveupdate

import android.content.Context
import com.dengage.sdk.util.DengageLogger
import java.util.Collections
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
 *                                  validate event vs active state
 *                                         ↓
 *                                  handler registry lookup by activity_type
 *                                         ↓
 *                                  LiveUpdateHandler.onUpdate(context, payload)
 * ```
 *
 * ### Event lifecycle
 * - **START** → registers activityId as active, dispatches to handler
 * - **UPDATE** → only dispatches if activityId is already active (ignores otherwise)
 * - **END** → only dispatches if activityId is already active, then removes it
 *
 * ### Expected FCM data payload
 * ```json
 * {
 *   "live_notification": "{\"activity_type\":\"delivery\",\"event\":\"start\",\"activityId\":\"uuid\",\"content_state\":{...}}"
 * }
 * ```
 */
object DengageLiveUpdateManager {

    private val handlers = ConcurrentHashMap<String, LiveUpdateHandler>()
    private val activeActivities = Collections.synchronizedSet(mutableSetOf<String>())

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

    /**
     * Returns `true` if the given activityId has an active live update session.
     */
    fun isActive(activityId: String): Boolean = activeActivities.contains(activityId)

    // -------------------------------------------------------------------------
    // FCM routing (called by FcmMessagingService)
    // -------------------------------------------------------------------------

    /**
     * Entry point for FCM messages that carry a `live_notification` key.
     * Parses the JSON, validates event lifecycle, resolves the handler, and dispatches.
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
        if (handler == null) {
            DengageLogger.verbose("LiveUpdate ignored — no handler for type: ${payload.activityType}")
            return
        }

        when (payload.event) {
            LiveUpdateEvent.START -> {
                activeActivities.add(payload.activityId)
                DengageLogger.verbose("LiveUpdate START → ${payload.activityType} [${payload.activityId}]")
                handler.onUpdate(context, payload)
            }

            LiveUpdateEvent.UPDATE -> {
                if (!activeActivities.contains(payload.activityId)) {
                    DengageLogger.verbose("LiveUpdate UPDATE ignored — not active: [${payload.activityId}]")
                    return
                }
                DengageLogger.verbose("LiveUpdate UPDATE → ${payload.activityType} [${payload.activityId}]")
                handler.onUpdate(context, payload)
            }

            LiveUpdateEvent.END -> {
                if (!activeActivities.contains(payload.activityId)) {
                    DengageLogger.verbose("LiveUpdate END ignored — not active: [${payload.activityId}]")
                    return
                }
                activeActivities.remove(payload.activityId)
                DengageLogger.verbose("LiveUpdate END → ${payload.activityType} [${payload.activityId}]")
                handler.onUpdate(context, payload)
            }
        }
    }
}
