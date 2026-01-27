package com.dengage.sdk.ui.inappmessage.bridge.handlers

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeCallback
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeMessage
import com.dengage.sdk.ui.inappmessage.bridge.handler.AsyncBridgeHandler
import com.dengage.sdk.ui.inappmessage.bridge.util.BridgeErrorCodes
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.GsonHolder
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.RequestBody.Companion.toRequestBody
import java.util.concurrent.TimeUnit

/**
 * Handler for HTTP requests from WebView
 */
class HttpRequestHandler(
    private val inAppMessage: InAppMessage? = null
) : AsyncBridgeHandler {

    private val client = OkHttpClient.Builder()
        .connectTimeout(30, TimeUnit.SECONDS)
        .readTimeout(30, TimeUnit.SECONDS)
        .writeTimeout(30, TimeUnit.SECONDS)
        .build()

    private val scope = CoroutineScope(Dispatchers.IO + SupervisorJob())

    data class HttpRequestPayload(
        val url: String,
        val method: String = "GET",
        val headers: Map<String, String>? = null,
        val body: String? = null,
        val queryParams: Map<String, String>? = null,
        val contentType: String? = null
    )

    data class HttpResponse(
        val statusCode: Int,
        val body: Any?,
        val headers: Map<String, String>
    )

    override fun supportedActions(): List<String> = listOf("httpRequest")

    override fun handle(message: BridgeMessage, callback: BridgeCallback<Any?>) {
        val payload = GsonHolder.fromJson<HttpRequestPayload>(message.payload)

        if (payload == null) {
            callback.onError(BridgeErrorCodes.INVALID_PAYLOAD, "Invalid HTTP request payload")
            return
        }

        scope.launch {
            try {
                val processedUrl = replaceVariables(payload.url)
                
                val httpUrlBuilder = processedUrl.toHttpUrlOrNull()?.newBuilder()
                    ?: throw IllegalArgumentException("Invalid URL: $processedUrl")
                
                payload.queryParams?.forEach { (key, value) ->
                    val processedValue = replaceVariables(value)
                    httpUrlBuilder.addQueryParameter(key, processedValue)
                }
                
                val finalUrl = httpUrlBuilder.build().toString()
                val requestBuilder = Request.Builder().url(finalUrl)

                payload.headers?.forEach { (key, value) ->
                    val processedValue = replaceVariables(value)
                    requestBuilder.addHeader(key, processedValue)
                }

                val bodyString = when {
                    payload.body != null -> {
                        replaceVariables(payload.body)
                    }
                    else -> null
                }

                when (payload.method.uppercase()) {
                    "GET" -> requestBuilder.get()
                    "POST" -> {
                        val mediaType = (payload.contentType ?: "application/json").toMediaType()
                        requestBuilder.post((bodyString ?: "").toRequestBody(mediaType))
                    }
                    "PUT" -> {
                        val mediaType = (payload.contentType ?: "application/json").toMediaType()
                        requestBuilder.put((bodyString ?: "").toRequestBody(mediaType))
                    }
                    "PATCH" -> {
                        val mediaType = (payload.contentType ?: "application/json").toMediaType()
                        requestBuilder.patch((bodyString ?: "").toRequestBody(mediaType))
                    }
                    "DELETE" -> requestBuilder.delete()
                    else -> {
                        withContext(Dispatchers.Main) {
                            callback.onError(BridgeErrorCodes.INVALID_PAYLOAD, "Unsupported HTTP method: ${payload.method}")
                        }
                        return@launch
                    }
                }

                val response = client.newCall(requestBuilder.build()).execute()
                val responseHeaders = response.headers.toMultimap()
                    .mapValues { it.value.firstOrNull() ?: "" }

                val rawBody = response.body?.string()
                val parsedBody: Any? = rawBody?.let { body ->
                    try {
                        GsonHolder.fromJson<Any>(body)
                    } catch (e: Exception) {
                        body
                    }
                }

                val httpResponse = HttpResponse(
                    statusCode = response.code,
                    body = parsedBody,
                    headers = responseHeaders
                )

                withContext(Dispatchers.Main) {
                    callback.onSuccess(httpResponse)
                }
            } catch (e: Exception) {
                DengageLogger.error("HttpRequestHandler error: ${e.message}")
                withContext(Dispatchers.Main) {
                    callback.onError(BridgeErrorCodes.HTTP_ERROR, e.message ?: "HTTP request failed")
                }
            }
        }
    }

    private fun replaceVariables(input: String): String {
        var result = input
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters
        result = result.replace("\${integrationKey}", subscription?.integrationKey ?: "")
        result = result.replace("\${accountName}", sdkParameters?.accountName ?: "")
        result = result.replace("\${appId}", sdkParameters?.appId ?: "")
        result = result.replace("\${campaign.publicId}", inAppMessage?.data?.publicId ?: "")
        result = result.replace("\${campaign.content.contentId}", inAppMessage?.data?.content?.contentId ?: "")
        result = result.replace("\${visitor.deviceId}", subscription?.getSafeDeviceId() ?: "")
        result = result.replace("\${visitor.contactKey}", subscription?.contactKey ?: "")
        result = result.replace("\${pushApiBaseUrl}", Prefs.pushApiBaseUrl)
        result = result.replace("\${eventApiBaseUrl}", Prefs.eventApiBaseUrl)
        result = result.replace("\${inAppApiBaseUrl}", Prefs.inAppApiBaseUrl)
        result = result.replace("\${geofenceApiBaseUrl}", Prefs.geofenceApiBaseUrl.removeSuffix("/"))
        result = result.replace("\${getRealTimeMessagesBaseUrl}", Prefs.getRealTimeMessagesBaseUrl.removeSuffix("/"))
        return result
    }
}
