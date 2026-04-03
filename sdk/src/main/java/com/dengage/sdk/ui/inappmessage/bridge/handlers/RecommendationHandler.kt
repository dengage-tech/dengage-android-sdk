package com.dengage.sdk.ui.inappmessage.bridge.handlers

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.manager.inappmessage.util.RealTimeInAppParamHolder
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeCallback
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeMessage
import com.dengage.sdk.ui.inappmessage.bridge.handler.AsyncBridgeHandler
import com.dengage.sdk.ui.inappmessage.bridge.util.BridgeErrorCodes
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.GsonHolder
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody
import java.util.concurrent.TimeUnit

/**
 * Handler for recommendation requests from WebView.
 * Collects user behavior data, sends a POST request to the Recommendation API,
 * and returns the recommendation response back to the bridge.
 */
class RecommendationHandler : AsyncBridgeHandler {

    private val client = OkHttpClient.Builder()
        .connectTimeout(30, TimeUnit.SECONDS)
        .readTimeout(30, TimeUnit.SECONDS)
        .writeTimeout(30, TimeUnit.SECONDS)
        .build()

    private val scope = CoroutineScope(Dispatchers.IO + SupervisorJob())

    override fun supportedActions(): List<String> = listOf("getRecommendation")

    override fun handle(message: BridgeMessage, callback: BridgeCallback<Any?>) {
        val payload = GsonHolder.fromJson<Map<String, Any>>(message.payload) ?: emptyMap()

        val containerKey = payload["containerKey"]?.toString()
        if (containerKey.isNullOrBlank()) {
            callback.onError(
                BridgeErrorCodes.INVALID_PAYLOAD,
                "containerKey is required in payload"
            )
            return
        }

        scope.launch {
            try {
                val requestBody = buildRequestBody(payload)
                val url = buildUrl(containerKey)

                val request = Request.Builder()
                    .url(url)
                    .addHeader("Content-Type", "application/json")
                    .post(requestBody.toRequestBody("application/json".toMediaType()))
                    .build()

                DengageLogger.debug("RecommendationHandler request URL: $url")

                val response = client.newCall(request).execute()
                val rawBody = response.body?.string()

                if (!response.isSuccessful) {
                    DengageLogger.error("RecommendationHandler error: ${response.code} - $rawBody")
                    withContext(Dispatchers.Main) {
                        callback.onError(
                            BridgeErrorCodes.HTTP_ERROR,
                            "Recommendation request failed with status ${response.code}: ${rawBody ?: ""}"
                        )
                    }
                    return@launch
                }

                val parsedBody: Any? = rawBody?.let { body ->
                    try {
                        GsonHolder.fromJson<Any>(body)
                    } catch (e: Exception) {
                        body
                    }
                }

                withContext(Dispatchers.Main) {
                    callback.onSuccess(parsedBody)
                }
            } catch (e: Exception) {
                DengageLogger.error("RecommendationHandler error: ${e.message}")
                withContext(Dispatchers.Main) {
                    callback.onError(
                        BridgeErrorCodes.HTTP_ERROR,
                        e.message ?: "Recommendation request failed"
                    )
                }
            }
        }
    }

    private fun buildUrl(containerKey: String): String {
        val baseUrl = Prefs.pushApiBaseUrl.removeSuffix("/")
        val accountId = Prefs.sdkParameters?.accountId ?: ""
        return "$baseUrl/rc/$accountId/recommendations/$containerKey/android"
    }

    private fun buildRequestBody(payload: Map<String, Any>): String {
        val cart = RealTimeInAppParamHolder.getCart()
        val cartProductIds = cart.items.map { it.productId }

        val body = mutableMapOf<String, Any?>()
        body["did"] = DengageUtils.getDeviceId()
        body["ck"] = Prefs.subscription?.contactKey
        body["cp"] = RealTimeInAppParamHolder.getLastProductId()
        body["cid"] = RealTimeInAppParamHolder.categoryPath
        body["cs"] = RealTimeInAppParamHolder.currentSearchWord

        if (cartProductIds.isNotEmpty()) body["crp"] = cartProductIds
        if (RealTimeInAppParamHolder.lastViewedProducts.isNotEmpty()) {
            body["lvp"] = RealTimeInAppParamHolder.lastViewedProducts
        }
        if (RealTimeInAppParamHolder.lastViewedCategories.isNotEmpty()) {
            body["lvc"] = RealTimeInAppParamHolder.lastViewedCategories
        }
        if (RealTimeInAppParamHolder.lastPurchasedProducts.isNotEmpty()) {
            body["lpp"] = RealTimeInAppParamHolder.lastPurchasedProducts
        }
        if (RealTimeInAppParamHolder.lastPurchasedCategories.isNotEmpty()) {
            body["lpc"] = RealTimeInAppParamHolder.lastPurchasedCategories
        }
        if (RealTimeInAppParamHolder.lastSearchWords.isNotEmpty()) {
            body["ls"] = RealTimeInAppParamHolder.lastSearchWords
        }

        // Add extra fields from payload (excluding containerKey)
        payload.forEach { (key, value) ->
            if (key != "containerKey") {
                body[key] = value
            }
        }

        return GsonHolder.toJson(body)
    }
}
