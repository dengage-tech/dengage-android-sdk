package com.dengage.sdk.ui.inappmessage.bridge.util

/**
 * Error codes for bridge operations
 */
object BridgeErrorCodes {
    // Handler errors
    const val HANDLER_NOT_FOUND = "HANDLER_NOT_FOUND"
    const val INVALID_PAYLOAD = "INVALID_PAYLOAD"
    const val UNKNOWN_ACTION = "UNKNOWN_ACTION"

    // Network errors
    const val HTTP_ERROR = "HTTP_ERROR"
    const val NETWORK_UNAVAILABLE = "NETWORK_UNAVAILABLE"
    const val TIMEOUT = "TIMEOUT"

    // Storage errors
    const val STORAGE_ERROR = "STORAGE_ERROR"
    const val KEY_NOT_FOUND = "KEY_NOT_FOUND"

    // Permission errors
    const val PERMISSION_DENIED = "PERMISSION_DENIED"
    const val PERMISSION_NOT_GRANTED = "PERMISSION_NOT_GRANTED"

    // General errors
    const val INTERNAL_ERROR = "INTERNAL_ERROR"
    const val CONTEXT_NULL = "CONTEXT_NULL"
    const val ACTIVITY_NULL = "ACTIVITY_NULL"
}
