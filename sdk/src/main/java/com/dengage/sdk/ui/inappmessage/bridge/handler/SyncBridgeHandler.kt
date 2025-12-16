package com.dengage.sdk.ui.inappmessage.bridge.handler

import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeMessage
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeResponse

/**
 * Handler interface for synchronous operations with immediate response
 */
interface SyncBridgeHandler : BridgeHandler {
    /**
     * Handle a sync message and return response immediately
     * @param message The bridge message to handle
     * @return BridgeResponse with the result
     */
    fun handleSync(message: BridgeMessage): BridgeResponse
}
