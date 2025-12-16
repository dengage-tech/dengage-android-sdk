package com.dengage.sdk.ui.inappmessage.bridge.js

import android.webkit.WebView

/**
 * JavaScript code to be injected into WebView for bridge communication
 */
object BridgeJavaScript {

    /**
     * JavaScript code that creates the DengageBridge interface on window
     */
    val BRIDGE_CODE = """
        (function() {
            // Prevent double initialization
            if (window.DengageBridge && window.DengageBridge._initialized) {
                return;
            }

            var callbackId = 0;
            var callbacks = {};

            window.DengageBridge = {
                _initialized: true,

                /**
                 * Fire and forget - no response expected
                 * @param {string} action - The action name
                 * @param {object} payload - The payload object
                 */
                fire: function(action, payload) {
                    var payloadJson = payload ? JSON.stringify(payload) : null;
                    if (window.DengageBridgeNative) {
                        window.DengageBridgeNative.fire(action, payloadJson);
                    }
                },

                /**
                 * Async call with callback
                 * @param {string} action - The action name
                 * @param {object} payload - The payload object
                 * @param {function} callback - Callback function(response)
                 */
                call: function(action, payload, callback) {
                    var id = 'cb_' + (++callbackId);
                    callbacks[id] = callback;
                    var payloadJson = payload ? JSON.stringify(payload) : null;
                    if (window.DengageBridgeNative) {
                        window.DengageBridgeNative.call(action, payloadJson, id);
                    }
                },

                /**
                 * Promise-based async call
                 * @param {string} action - The action name
                 * @param {object} payload - The payload object
                 * @returns {Promise} - Resolves with response data
                 */
                callAsync: function(action, payload) {
                    var self = this;
                    return new Promise(function(resolve, reject) {
                        self.call(action, payload, function(response) {
                            if (response.success) {
                                resolve(response.data);
                            } else {
                                reject({
                                    code: response.errorCode,
                                    message: response.errorMessage
                                });
                            }
                        });
                    });
                },

                /**
                 * Synchronous call - blocks until response
                 * @param {string} action - The action name
                 * @param {object} payload - The payload object
                 * @returns {object} - Response object
                 */
                callSync: function(action, payload) {
                    var payloadJson = payload ? JSON.stringify(payload) : null;
                    if (window.DengageBridgeNative) {
                        var responseJson = window.DengageBridgeNative.callSync(action, payloadJson);
                        return JSON.parse(responseJson);
                    }
                    return { success: false, errorCode: 'BRIDGE_NOT_READY', errorMessage: 'Bridge not initialized' };
                },

                /**
                 * Internal: Handle native response
                 * @param {object} response - The response from native
                 */
                _handleNativeResponse: function(response) {
                    var callback = callbacks[response.callId];
                    if (callback) {
                        delete callbacks[response.callId];
                        try {
                            callback(response);
                        } catch (e) {
                            console.error('DengageBridge callback error:', e);
                        }
                    }
                }
            };

            // Legacy Dn compatibility layer - only create if not already defined by native
            if (!window.Dn || !window.Dn._nativeInterface) {
                window.Dn = window.Dn || {};

                window.Dn.dismiss = function() {
                    DengageBridge.fire('legacy_dismiss', {});
                };

                window.Dn.androidUrl = function(targetUrl) {
                    DengageBridge.fire('legacy_androidUrl', { targetUrl: targetUrl });
                };

                window.Dn.androidUrlN = function(targetUrl, inAppBrowser, retrieveOnSameLink) {
                    DengageBridge.fire('legacy_androidUrlN', {
                        targetUrl: targetUrl,
                        inAppBrowser: inAppBrowser,
                        retrieveOnSameLink: retrieveOnSameLink
                    });
                };

                window.Dn.sendClick = function(buttonId) {
                    DengageBridge.fire('legacy_sendClick', { buttonId: buttonId });
                };

                window.Dn.close = function() {
                    DengageBridge.fire('legacy_close', {});
                };

                window.Dn.closeN = function() {
                    DengageBridge.fire('legacy_closeN', {});
                };

                window.Dn.setTags = function() {
                    DengageBridge.fire('legacy_setTags', {});
                };

                window.Dn.iosUrl = function(targetUrl) {
                    DengageBridge.fire('legacy_iosUrl', { targetUrl: targetUrl });
                };

                window.Dn.iosUrlN = function(targetUrl, inAppBrowser, retrieveOnSameLink) {
                    DengageBridge.fire('legacy_iosUrlN', {
                        targetUrl: targetUrl,
                        inAppBrowser: inAppBrowser,
                        retrieveOnSameLink: retrieveOnSameLink
                    });
                };

                window.Dn.promptPushPermission = function() {
                    DengageBridge.fire('legacy_promptPushPermission', {});
                };

                window.Dn.showRating = function() {
                    DengageBridge.fire('legacy_showRating', {});
                };
            }

            console.log('DengageBridge initialized');
        })();
    """.trimIndent()

    /**
     * Inject the bridge JavaScript into a WebView
     * @param webView The WebView to inject into
     */
    fun inject(webView: WebView) {
        webView.evaluateJavascript(BRIDGE_CODE, null)
    }
}
