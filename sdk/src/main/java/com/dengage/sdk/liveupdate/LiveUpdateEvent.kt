package com.dengage.sdk.liveupdate

/**
 * Represents the lifecycle events of a live update.
 */
public enum class LiveUpdateEvent {
    /** A new live update session has started. */
    START,
    /** The live update content has been updated. */
    UPDATE,
    /** The live update session has ended (dismiss). */
    END
}
