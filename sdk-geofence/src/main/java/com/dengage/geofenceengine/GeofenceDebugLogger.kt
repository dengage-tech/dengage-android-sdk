package com.dengage.geofenceengine

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.DebugLogRequest
import com.dengage.sdk.domain.inappmessage.DebugLoggingRepository
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import java.util.UUID

/**
 * Geofence engine hata loglaması (felogging → Graylog). Yalnızca server `sdkErrorLoggingEnabled == true`
 * iken gönderilir. In-app debug log ile aynı `/felogging/{screenName}` pipeline'ını kullanır.
 * Fire-and-forget: her yerden (suspend olmayan callback/receiver dahil) güvenle çağrılabilir.
 */
internal object GeofenceDebugLogger {

    private const val SCREEN_NAME = "geofence-engine"
    private val repository = DebugLoggingRepository()
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)

    fun error(message: String, context: Map<String, String> = emptyMap()) {
        val params = Prefs.sdkParameters
        if (params?.sdkErrorLoggingEnabled != true) return
        val subscription = Prefs.subscription

        scope.launch {
            try {
                val request = DebugLogRequest(
                    traceId = UUID.randomUUID().toString(),
                    appGuid = params.appId,
                    appId = params.appId,
                    account = params.accountName,
                    device = subscription?.getSafeDeviceId() ?: "",
                    sessionId = "",
                    sdkVersion = DengageUtils.getSdkVersion(),
                    currentCampaignList = emptyList(),
                    campaignId = null,
                    campaignType = null,
                    sendId = null,
                    message = message,
                    context = context,
                    contactKey = subscription?.contactKey,
                    channel = "android",
                    currentRules = mapOf()
                )
                repository.sendDebugLog(SCREEN_NAME, request)
            } catch (e: Exception) {
                DengageLogger.error("GeofenceDebugLogger -> send failed: ${e.message}")
            }
        }
    }
}
