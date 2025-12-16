package com.dengage.sdk.ui.inappmessage.bridge.handler

import java.util.concurrent.ConcurrentHashMap

/**
 * Registry for managing bridge handlers
 * Thread-safe implementation using ConcurrentHashMap
 */
class BridgeHandlerRegistry {
    private val handlers = ConcurrentHashMap<String, BridgeHandler>()

    companion object {
        /**
         * Default registry instance with standard handlers
         */
        val default: BridgeHandlerRegistry by lazy {
            BridgeHandlerRegistry()
        }
    }

    /**
     * Register a handler for its supported actions
     * @param handler The handler to register
     */
    fun register(handler: BridgeHandler) {
        handler.supportedActions().forEach { action ->
            handlers[action] = handler
        }
    }

    /**
     * Unregister a handler for a specific action
     * @param action The action to unregister
     */
    fun unregister(action: String) {
        handlers.remove(action)
    }

    /**
     * Get the handler for a specific action
     * @param action The action to get handler for
     * @return The handler or null if not found
     */
    fun getHandler(action: String): BridgeHandler? {
        return handlers[action]
    }

    /**
     * Check if a handler exists for a specific action
     * @param action The action to check
     * @return true if handler exists
     */
    fun hasHandler(action: String): Boolean {
        return handlers.containsKey(action)
    }

    /**
     * Get all registered actions
     * @return Set of all registered action names
     */
    fun getRegisteredActions(): Set<String> {
        return handlers.keys.toSet()
    }

    /**
     * Clear all registered handlers
     */
    fun clear() {
        handlers.clear()
    }
}
