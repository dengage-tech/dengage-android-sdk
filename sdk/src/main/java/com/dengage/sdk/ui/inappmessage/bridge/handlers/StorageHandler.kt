package com.dengage.sdk.ui.inappmessage.bridge.handlers

import android.content.Context
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeCallback
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeMessage
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeResponse
import com.dengage.sdk.ui.inappmessage.bridge.handler.AsyncBridgeHandler
import com.dengage.sdk.ui.inappmessage.bridge.handler.SyncBridgeHandler
import com.dengage.sdk.ui.inappmessage.bridge.util.BridgeErrorCodes
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.GsonHolder

/**
 * Handler for storage operations from WebView
 * Provides key-value storage using SharedPreferences
 */
class StorageHandler : SyncBridgeHandler, AsyncBridgeHandler {

    companion object {
        private const val BRIDGE_PREFS_NAME = "dengage_bridge_storage"
    }

    private val prefs by lazy {
        ContextHolder.context.getSharedPreferences(BRIDGE_PREFS_NAME, Context.MODE_PRIVATE)
    }

    data class SetStoragePayload(val key: String, val value: String?)
    data class GetStoragePayload(val key: String)
    data class RemoveStoragePayload(val key: String)

    override fun supportedActions(): List<String> = listOf(
        "storage_get",
        "storage_set",
        "storage_remove",
        "storage_clear",
        "storage_getAll"
    )

    // Sync implementation for read operations
    override fun handleSync(message: BridgeMessage): BridgeResponse {
        return try {
            when (message.action) {
                "storage_get" -> {
                    val payload = GsonHolder.fromJson<GetStoragePayload>(message.payload)
                    if (payload == null) {
                        BridgeResponse.error(message.callId, BridgeErrorCodes.INVALID_PAYLOAD, "Invalid payload")
                    } else {
                        BridgeResponse.success(message.callId, prefs.getString(payload.key, null))
                    }
                }
                "storage_getAll" -> {
                    val allValues = prefs.all.mapValues { it.value?.toString() }
                    BridgeResponse.success(message.callId, allValues)
                }
                else -> BridgeResponse.error(message.callId, BridgeErrorCodes.UNKNOWN_ACTION, "Use async for this action")
            }
        } catch (e: Exception) {
            DengageLogger.error("StorageHandler sync error: ${e.message}")
            BridgeResponse.error(message.callId, BridgeErrorCodes.STORAGE_ERROR, e.message ?: "Storage error")
        }
    }

    // Async implementation for write operations
    override fun handle(message: BridgeMessage, callback: BridgeCallback<Any?>) {
        try {
            when (message.action) {
                "storage_set" -> {
                    val payload = GsonHolder.fromJson<SetStoragePayload>(message.payload)
                    if (payload == null) {
                        callback.onError(BridgeErrorCodes.INVALID_PAYLOAD, "Invalid payload")
                    } else {
                        prefs.edit().putString(payload.key, payload.value).apply()
                        callback.onSuccess(true)
                    }
                }
                "storage_remove" -> {
                    val payload = GsonHolder.fromJson<RemoveStoragePayload>(message.payload)
                    if (payload == null) {
                        callback.onError(BridgeErrorCodes.INVALID_PAYLOAD, "Invalid payload")
                    } else {
                        prefs.edit().remove(payload.key).apply()
                        callback.onSuccess(true)
                    }
                }
                "storage_clear" -> {
                    prefs.edit().clear().apply()
                    callback.onSuccess(true)
                }
                "storage_get" -> {
                    val payload = GsonHolder.fromJson<GetStoragePayload>(message.payload)
                    if (payload == null) {
                        callback.onError(BridgeErrorCodes.INVALID_PAYLOAD, "Invalid payload")
                    } else {
                        callback.onSuccess(prefs.getString(payload.key, null))
                    }
                }
                "storage_getAll" -> {
                    val allValues = prefs.all.mapValues { it.value?.toString() }
                    callback.onSuccess(allValues)
                }
                else -> callback.onError(BridgeErrorCodes.UNKNOWN_ACTION, "Unknown storage action")
            }
        } catch (e: Exception) {
            DengageLogger.error("StorageHandler async error: ${e.message}")
            callback.onError(BridgeErrorCodes.STORAGE_ERROR, e.message ?: "Storage error")
        }
    }
}
