package com.dengage.sdk.ui.inappmessage.bridge.core

import java.io.Serializable

/**
 * Represents a response from Native to JavaScript
 */
data class BridgeResponse(
    val callId: String,
    val success: Boolean,
    val data: Any?,
    val errorCode: String?,
    val errorMessage: String?
) : Serializable {

    companion object {
        fun success(callId: String, data: Any? = null): BridgeResponse {
            return BridgeResponse(
                callId = callId,
                success = true,
                data = data,
                errorCode = null,
                errorMessage = null
            )
        }

        fun error(callId: String, errorCode: String, errorMessage: String): BridgeResponse {
            return BridgeResponse(
                callId = callId,
                success = false,
                data = null,
                errorCode = errorCode,
                errorMessage = errorMessage
            )
        }
    }
}
