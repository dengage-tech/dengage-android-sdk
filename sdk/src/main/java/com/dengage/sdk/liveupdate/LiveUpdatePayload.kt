package com.dengage.sdk.liveupdate

import com.dengage.sdk.util.DengageLogger
import org.json.JSONObject

/**
 * Parsed representation of a `live_notification` FCM data payload.
 *
 * ### Expected JSON structure
 * ```json
 * {
 *   "activity_type": "delivery",
 *   "event": "update",
 *   "activityId": "90db7e2a-5839-53cd-605f-9d3ffc328e21",
 *   "dismissal_date": 1774888005,
 *   "content_state": {
 *     "order_id": "DNG-8821",
 *     "delivery_status": "ON_THE_WAY",
 *     "estimated_time": "10 dk"
 *   }
 * }
 * ```
 */
data class LiveUpdatePayload(
    /** Handler type (e.g. "delivery", "sports") */
    val activityType: String,
    /** Lifecycle event */
    val event: LiveUpdateEvent,
    /** Unique ID — same activityId updates the same notification */
    val activityId: String,
    /** Content key-value pairs from `content_state` */
    val contentState: Map<String, String>,
    /**
     * Optional auto-dismiss timestamp in seconds (epoch).
     * When non-null, the notification should be automatically dismissed at this time.
     */
    val dismissalDate: Long? = null
) {
    companion object {
        /**
         * Parse from the raw JSON string found in `data["live_notification"]`.
         * Returns `null` if the JSON is invalid or missing required fields.
         */
        fun fromJson(json: String): LiveUpdatePayload? {
            return try {
                val obj = JSONObject(json)
                val activityType = obj.getString("activity_type")
                val activityId = obj.optString("activityId", "")

                val event = when (obj.optString("event", "update")) {
                    "start" -> LiveUpdateEvent.START
                    "end"   -> LiveUpdateEvent.END
                    else    -> LiveUpdateEvent.UPDATE
                }

                val contentState = mutableMapOf<String, String>()
                val cs = obj.optJSONObject("content_state")
                if (cs != null) {
                    val keys = cs.keys()
                    while (keys.hasNext()) {
                        val key = keys.next()
                        contentState[key] = cs.optString(key, "")
                    }
                }

                val dismissalDate = if (obj.has("dismissal_date")) {
                    obj.optLong("dismissal_date", 0L).let { if (it > 0) it else null }
                } else null

                LiveUpdatePayload(activityType, event, activityId, contentState, dismissalDate)
            } catch (e: Exception) {
                DengageLogger.error("LiveUpdatePayload parse error: ${e.message}")
                null
            }
        }
    }
}
