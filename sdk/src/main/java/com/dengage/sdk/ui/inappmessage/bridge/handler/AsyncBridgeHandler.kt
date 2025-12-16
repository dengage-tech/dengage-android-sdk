package com.dengage.sdk.ui.inappmessage.bridge.handler

import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeCallback
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeMessage

/**
 * Handler interface for async operations with callback response
 */
interface AsyncBridgeHandler : BridgeHandler {
    /**
     * Handle an async message with callback
     * @param message The bridge message to handle
     * @param callback Callback to invoke when operation completes
     */
    fun handle(message: BridgeMessage, callback: BridgeCallback<Any?>)
}
