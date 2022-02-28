package com.dengage.sdk.data.remote.api

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.data.remote.provider.ConverterFactoryProvider
import com.dengage.sdk.data.remote.provider.InterceptorProvider
import com.dengage.sdk.data.remote.provider.UrlProvider
import okhttp3.OkHttpClient
import retrofit2.Retrofit
import java.util.concurrent.TimeUnit

class PushApiProvider {

    private val urlProvider = UrlProvider(
        baseUrl = Prefs.pushApiBaseUrl
    )
    private val interceptorProvider = InterceptorProvider()

    private val retrofit: Retrofit by lazy {
        Retrofit.Builder()
            .baseUrl(urlProvider.url)
            .apply {
                client(getOkHttpClientBuilder().build())
            }
            .apply {
                for (converterFactory in ConverterFactoryProvider().converterFactories) {
                    addConverterFactory(converterFactory)
                }
            }
            .build()
    }

    private fun getOkHttpClientBuilder(): OkHttpClient.Builder {
        return OkHttpClient.Builder()
            .connectTimeout(CONNECT_TIME_OUT, TimeUnit.SECONDS)
            .readTimeout(READ_TIME_OUT, TimeUnit.SECONDS)
            .apply {
                for (interceptor in interceptorProvider.interceptors) {
                    addInterceptor(interceptor)
                }
            }
    }

    fun <S> create(serviceClass: Class<S>): S {
        return retrofit.create(serviceClass)
    }

    companion object {
        private const val CONNECT_TIME_OUT = 20L
        private const val READ_TIME_OUT = 20L

        var INSTANCE = create()

        private fun create() = PushApiProvider()
    }
}