package com.dengage.sdk.ui.inappmessage.bridge.handlers

import android.os.Build
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeMessage
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeResponse
import com.dengage.sdk.ui.inappmessage.bridge.handler.SyncBridgeHandler
import com.dengage.sdk.ui.inappmessage.bridge.util.BridgeErrorCodes
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageUtils

/**
 * Handler for device information requests from WebView
 */
class DeviceInfoHandler : SyncBridgeHandler {

    data class DeviceInfo(
        val deviceId: String,
        val sdkVersion: String,
        val osVersion: String,
        val manufacturer: String,
        val model: String,
        val appVersion: String?,
        val token: String?,
        val contactKey: String?,
        val language: String
    )

    override fun supportedActions(): List<String> = listOf(
        "getDeviceId",
        "getDeviceInfo",
        "getSdkVersion",
        "getToken",
        "getContactKey"
    )

    override fun handleSync(message: BridgeMessage): BridgeResponse {
        return try {
            when (message.action) {
                "getDeviceId" -> {
                    BridgeResponse.success(message.callId, DengageUtils.getDeviceId())
                }
                "getSdkVersion" -> {
                    BridgeResponse.success(message.callId, DengageUtils.getSdkVersion())
                }
                "getToken" -> {
                    BridgeResponse.success(message.callId, Prefs.subscription?.token)
                }
                "getContactKey" -> {
                    BridgeResponse.success(message.callId, Prefs.subscription?.contactKey)
                }
                "getDeviceInfo" -> {
                    val deviceInfo = DeviceInfo(
                        deviceId = DengageUtils.getDeviceId(),
                        sdkVersion = DengageUtils.getSdkVersion(),
                        osVersion = Build.VERSION.RELEASE,
                        manufacturer = Build.MANUFACTURER,
                        model = Build.MODEL,
                        appVersion = DengageUtils.getAppVersion(ContextHolder.context),
                        token = Prefs.subscription?.token,
                        contactKey = Prefs.subscription?.contactKey,
                        language = DengageUtils.getLanguage()
                    )
                    BridgeResponse.success(message.callId, deviceInfo)
                }
                else -> BridgeResponse.error(message.callId, BridgeErrorCodes.UNKNOWN_ACTION, "Unknown action: ${message.action}")
            }
        } catch (e: Exception) {
            BridgeResponse.error(message.callId, BridgeErrorCodes.INTERNAL_ERROR, e.message ?: "Unknown error")
        }
    }
}
