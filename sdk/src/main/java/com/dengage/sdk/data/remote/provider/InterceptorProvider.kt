package com.dengage.sdk.data.remote.provider

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageUtils
import okhttp3.Interceptor
import okhttp3.MediaType.Companion.toMediaTypeOrNull
import okhttp3.Response
import okhttp3.ResponseBody.Companion.toResponseBody
import okhttp3.logging.HttpLoggingInterceptor
import java.util.*

class InterceptorProvider {

    val interceptors: List<Interceptor>
        get() = createLoggingInterceptors() + arrayListOf(
            createHeaderInterceptor(),
            createEmptyBodyInterceptor()
        )

    private fun createHeaderInterceptor(): Interceptor {
        return Interceptor { chain ->
            val originalRequest = chain.request()

            val newRequestBuilder = originalRequest.newBuilder()

            for (entry in getHeaders().entries) {
                newRequestBuilder.addHeader(entry.key, entry.value)
            }

            chain.proceed(newRequestBuilder.build())
        }
    }

    private fun createLoggingInterceptors(): List<Interceptor> {
        return arrayListOf(
            if (Prefs.logVisibility) {
                HttpLoggingInterceptor().apply {
                    level = HttpLoggingInterceptor.Level.BODY
                }
            } else {
                HttpLoggingInterceptor().apply {
                    level = HttpLoggingInterceptor.Level.NONE
                }
            }
        )
    }

    private fun createEmptyBodyInterceptor(): Interceptor {
        return object : Interceptor {
            override fun intercept(chain: Interceptor.Chain): Response {
                val response = chain.proceed(chain.request())
                val body = response.body
                val content = body?.string()
                val contentType = body?.contentType()
                if (response.isSuccessful) {
                    if (content?.equals("") == true || content?.equals("null") == true) {

                        val emptyBody = "{}"
                            .toResponseBody("text/plain".toMediaTypeOrNull())
                        return response
                            .newBuilder()
                            .code(200)
                            .body(emptyBody)
                            .build()
                    }
                }

                return response.newBuilder()
                    .body(content?.toResponseBody(contentType))
                    .build()
            }
        }
    }

    private fun getHeaders(): Map<String, String> {
        val headers = HashMap<String, String>().apply {
            put("Cache-Control", "no-cache")
            put("Content-Type", "application/json")
            put("User-Agent", DengageUtils.getUserAgent(ContextHolder.context))
        }
        return headers
    }

}
