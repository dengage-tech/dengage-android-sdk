package com.dengage.sdk.ui.inappmessage.bridge.core

import android.os.Handler
import android.os.Looper
import android.webkit.JavascriptInterface
import android.webkit.WebView
import com.dengage.sdk.ui.inappmessage.bridge.handler.AsyncBridgeHandler
import com.dengage.sdk.ui.inappmessage.bridge.handler.BridgeHandlerRegistry
import com.dengage.sdk.ui.inappmessage.bridge.handler.FireAndForgetHandler
import com.dengage.sdk.ui.inappmessage.bridge.handler.SyncBridgeHandler
import com.dengage.sdk.ui.inappmessage.bridge.util.BridgeErrorCodes
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.GsonHolder
import java.lang.ref.WeakReference

/**
 * Main bridge class that exposes native functionality to WebView JavaScript
 */
class DengageBridge private constructor(
    webView: WebView,
    private val handlerRegistry: BridgeHandlerRegistry
) {
    private val webViewRef = WeakReference(webView)
    private val mainHandler = Handler(Looper.getMainLooper())

    companion object {
        const val BRIDGE_NAME = "DengageBridgeNative"

        /**
         * Attach the bridge to a WebView
         * @param webView The WebView to attach to
         * @param handlerRegistry Optional custom handler registry
         * @return DengageBridge instance
         */
        fun attach(
            webView: WebView,
            handlerRegistry: BridgeHandlerRegistry = BridgeHandlerRegistry.default
        ): DengageBridge {
            val bridge = DengageBridge(webView, handlerRegistry)
            webView.addJavascriptInterface(bridge, BRIDGE_NAME)
            return bridge
        }
    }

    /**
     * Fire and forget - no response callback
     * Called from JS: DengageBridge.fire("action", {payload})
     */
    @JavascriptInterface
    fun fire(action: String, payloadJson: String?) {
        DengageLogger.verbose("Bridge fire: action=$action")
        try {
            val message = BridgeMessage(
                callId = "",
                action = action,
                payload = payloadJson,
                type = BridgeMessage.MessageType.FIRE_FORGET
            )

            val handler = handlerRegistry.getHandler(action)
            if (handler is FireAndForgetHandler) {
                handler.handle(message)
            } else {
                DengageLogger.error("No fire-and-forget handler found for action: $action")
            }
        } catch (e: Exception) {
            DengageLogger.error("Bridge fire error: ${e.message}")
        }
    }

    /**
     * Async call with callback
     * Called from JS: DengageBridge.call("action", {payload}, callbackId)
     */
    @JavascriptInterface
    fun call(action: String, payloadJson: String?, callId: String) {
        DengageLogger.verbose("Bridge call: action=$action, callId=$callId")
        try {
            val message = BridgeMessage(
                callId = callId,
                action = action,
                payload = payloadJson,
                type = BridgeMessage.MessageType.ASYNC
            )

            val handler = handlerRegistry.getHandler(action)
            if (handler is AsyncBridgeHandler) {
                handler.handle(message, object : BridgeCallback<Any?> {
                    override fun onSuccess(data: Any?) {
                        sendResponse(BridgeResponse.success(callId, data))
                    }

                    override fun onError(errorCode: String, errorMessage: String) {
                        sendResponse(BridgeResponse.error(callId, errorCode, errorMessage))
                    }
                })
            } else {
                sendResponse(
                    BridgeResponse.error(
                        callId,
                        BridgeErrorCodes.HANDLER_NOT_FOUND,
                        "No async handler found for action: $action"
                    )
                )
            }
        } catch (e: Exception) {
            DengageLogger.error("Bridge call error: ${e.message}")
            sendResponse(
                BridgeResponse.error(
                    callId,
                    BridgeErrorCodes.INTERNAL_ERROR,
                    e.message ?: "Unknown error"
                )
            )
        }
    }

    /**
     * Synchronous call - returns immediately
     * Called from JS: DengageBridge.callSync("action", {payload})
     */
    @JavascriptInterface
    fun callSync(action: String, payloadJson: String?): String {
        DengageLogger.verbose("Bridge callSync: action=$action")
        return try {
            val message = BridgeMessage(
                callId = "",
                action = action,
                payload = payloadJson,
                type = BridgeMessage.MessageType.SYNC
            )

            val handler = handlerRegistry.getHandler(action)
            if (handler is SyncBridgeHandler) {
                val response = handler.handleSync(message)
                GsonHolder.toJson(response)
            } else {
                GsonHolder.toJson(
                    BridgeResponse.error(
                        "",
                        BridgeErrorCodes.HANDLER_NOT_FOUND,
                        "No sync handler found for action: $action"
                    )
                )
            }
        } catch (e: Exception) {
            DengageLogger.error("Bridge callSync error: ${e.message}")
            GsonHolder.toJson(
                BridgeResponse.error(
                    "",
                    BridgeErrorCodes.INTERNAL_ERROR,
                    e.message ?: "Unknown error"
                )
            )
        }
    }

    /**
     * Send response back to JavaScript
     */
    private fun sendResponse(response: BridgeResponse) {
        val responseJson = GsonHolder.toJson(response)
        val js = "window.DengageBridge && window.DengageBridge._handleNativeResponse($responseJson)"

        mainHandler.post {
            webViewRef.get()?.evaluateJavascript(js, null)
        }
    }

    /**
     * Get the handler registry
     */
    fun getHandlerRegistry(): BridgeHandlerRegistry = handlerRegistry
}
