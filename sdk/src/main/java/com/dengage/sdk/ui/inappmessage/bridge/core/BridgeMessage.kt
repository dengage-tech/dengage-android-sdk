package com.dengage.sdk.ui.inappmessage.bridge.core

import java.io.Serializable

/**
 * Represents a message from JavaScript to Native
 */
data class BridgeMessage(
    val callId: String,
    val action: String,
    val payload: String?,
    val type: MessageType
) : Serializable {

    enum class MessageType {
        FIRE_FORGET,  // No response needed
        ASYNC,        // Callback response
        SYNC          // Blocking response
    }
}
