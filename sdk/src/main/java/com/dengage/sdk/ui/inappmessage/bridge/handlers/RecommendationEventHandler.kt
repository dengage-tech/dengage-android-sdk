package com.dengage.sdk.ui.inappmessage.bridge.handlers

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.manager.session.SessionManager
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeCallback
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeMessage
import com.dengage.sdk.ui.inappmessage.bridge.handler.AsyncBridgeHandler
import com.dengage.sdk.ui.inappmessage.bridge.util.BridgeErrorCodes
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.GsonHolder
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody
import java.util.concurrent.TimeUnit

class RecommendationEventHandler : AsyncBridgeHandler {

    private data class Batch(
        val id: String,
        val payload: MutableMap<String, Any?>,
        val events: MutableList<MutableMap<String, Any?>>,
        var flushJob: Job? = null
    )

    private val client = OkHttpClient.Builder()
        .connectTimeout(30, TimeUnit.SECONDS)
        .readTimeout(30, TimeUnit.SECONDS)
        .writeTimeout(30, TimeUnit.SECONDS)
        .build()

    private val scope = CoroutineScope(Dispatchers.IO + SupervisorJob())

    private val batchesLock = Any()
    private val batches = mutableMapOf<String, Batch>()

    override fun supportedActions(): List<String> = listOf(
        "sendRecommendationImpressionEvent",
        "sendRecommendationClickEvent"
    )

    override fun handle(message: BridgeMessage, callback: BridgeCallback<Any?>) {
        try {
            val args = parseArgs(message.payload)
            if (args.size < 3) {
                callback.onError(
                    BridgeErrorCodes.INVALID_PAYLOAD,
                    "Expected (recommendationRequestId, containerKey, itemIds)"
                )
                return
            }

            val eventType = when (message.action) {
                "sendRecommendationImpressionEvent" -> "IM"
                "sendRecommendationClickEvent" -> "CL"
                else -> {
                    callback.onError(BridgeErrorCodes.INVALID_PAYLOAD, "Unknown action ${message.action}")
                    return
                }
            }

            val recommendationRequestId = args[0]?.toString()
            val containerKey = args[1]?.toString()
            val rawItemIds = args[2]

            val event = buildEvent(eventType, recommendationRequestId, containerKey, rawItemIds)

            enqueue(event)

            callback.onSuccess(true)
        } catch (e: Exception) {
            DengageLogger.error("RecommendationEventHandler error: ${e.message}")
            callback.onError(
                BridgeErrorCodes.INTERNAL_ERROR,
                e.message ?: "Failed to enqueue recommendation event"
            )
        }
    }

    private fun parseArgs(payloadJson: String?): List<Any?> {
        if (payloadJson.isNullOrBlank()) return emptyList()
        val payload = GsonHolder.fromJson<Map<String, Any?>>(payloadJson) ?: return emptyList()
        @Suppress("UNCHECKED_CAST")
        return (payload["args"] as? List<Any?>) ?: emptyList()
    }

    private fun buildEvent(
        eventType: String,
        recommendationRequestId: String?,
        containerKey: String?,
        rawItemIds: Any?
    ): MutableMap<String, Any?> {
        val event = mutableMapOf<String, Any?>(
            "t" to eventType,
            "rid" to recommendationRequestId,
            "ck2" to containerKey
        )

        val itemIdList: List<Any?> = when (rawItemIds) {
            null -> emptyList()
            is List<*> -> rawItemIds
            else -> listOf(rawItemIds)
        }

        val isSingleItem = itemIdList.size == 1

        if (eventType == "IM" || !isSingleItem) {
            event["pids"] = itemIdList.map { it?.toString() }
        } else if (itemIdList.isNotEmpty()) {
            event["pid"] = itemIdList[0]?.toString()
        }

        return event
    }

    private fun enqueue(event: MutableMap<String, Any?>) {
        val deviceId = DengageUtils.getDeviceId()
        val contactKey = Prefs.subscription?.contactKey ?: ""
        val sessionId: String = try {
            SessionManager.getSessionId()
        } catch (e: Exception) {
            ""
        }

        val batchId = listOf(deviceId, contactKey, sessionId).joinToString("_")

        event["ts"] = System.currentTimeMillis()

        val batchToFlush: Batch
        synchronized(batchesLock) {
            val batch = batches.getOrPut(batchId) {
                Batch(
                    id = batchId,
                    payload = mutableMapOf(
                        "p" to "android",
                        "did" to deviceId,
                        "ck" to contactKey.ifEmpty { null },
                        "sid" to sessionId.ifEmpty { null },
                        "events" to mutableListOf<Map<String, Any?>>()
                    ),
                    events = mutableListOf()
                )
            }

            val alreadyQueued = batch.events.any { it == event }
            if (!alreadyQueued) {
                batch.events.add(event)
            }

            batch.flushJob?.cancel()
            batchToFlush = batch
            batch.flushJob = scope.launch {
                delay(BATCH_FLUSH_DELAY_MS)
                flush(batchToFlush.id)
            }
        }
    }

    private fun flush(batchId: String) {
        val batch: Batch
        synchronized(batchesLock) {
            batch = batches.remove(batchId) ?: return
        }

        if (batch.events.isEmpty()) return

        val payload = batch.payload.toMutableMap()
        payload["events"] = batch.events.toList()

        val accountName = Prefs.sdkParameters?.accountName
        if (accountName.isNullOrBlank()) {
            DengageLogger.error("RecommendationEventHandler: accountName missing, dropping batch")
            return
        }

        val baseUrl = Prefs.pushApiBaseUrl.removeSuffix("/")
        val url = "$baseUrl/re/$accountName/reco-events/batch"
        val body = GsonHolder.toJson(payload)

        try {
            val request = Request.Builder()
                .url(url)
                .addHeader("Content-Type", "application/json")
                .post(body.toRequestBody("application/json".toMediaType()))
                .build()

            DengageLogger.debug("RecommendationEventHandler flush URL: $url")
            DengageLogger.debug("RecommendationEventHandler flush body: $body")

            client.newCall(request).execute().use { response ->
                if (!response.isSuccessful) {
                    DengageLogger.error(
                        "RecommendationEventHandler flush failed: ${response.code} - ${response.body?.string()}"
                    )
                }
            }
        } catch (e: Exception) {
            DengageLogger.error("RecommendationEventHandler flush error: ${e.message}")
        }
    }

    companion object {
        private const val BATCH_FLUSH_DELAY_MS = 500L
    }
}
