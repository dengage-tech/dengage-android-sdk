package com.dengage.sdk.ui.inappmessage.bridge.handler

/**
 * Base interface for all bridge handlers
 */
interface BridgeHandler {
    /**
     * Returns the list of actions this handler supports
     */
    fun supportedActions(): List<String>
}
