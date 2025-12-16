package com.dengage.sdk.ui.inappmessage.bridge.handler

import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeMessage

/**
 * Handler interface for fire-and-forget operations (no response needed)
 */
interface FireAndForgetHandler : BridgeHandler {
    /**
     * Handle a fire-and-forget message
     * @param message The bridge message to handle
     */
    fun handle(message: BridgeMessage)
}
