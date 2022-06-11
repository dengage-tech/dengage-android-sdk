package com.dengage.sdk.data.remote.api

import java.util.concurrent.TimeUnit
import okhttp3.OkHttpClient
import retrofit2.Retrofit
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.data.remote.provider.ConverterFactoryProvider
import com.dengage.sdk.data.remote.provider.InterceptorProvider
import com.dengage.sdk.data.remote.provider.UrlProvider

class GeofenceApiProvider {

    private val urlProvider = UrlProvider(
        baseUrl = Prefs.geofenceApiBaseUrl
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
            .connectTimeout(GeofenceApiProvider.CONNECT_TIME_OUT, TimeUnit.SECONDS)
            .readTimeout(GeofenceApiProvider.READ_TIME_OUT, TimeUnit.SECONDS)
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

        private fun create() = GeofenceApiProvider()
    }

}