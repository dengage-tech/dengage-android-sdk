package com.dengage.android.kotlin.sample.network


import okhttp3.OkHttpClient
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import java.util.concurrent.TimeUnit


class RetrofitClient private constructor() {
    private val myApi: Api

    init {
        val retrofit: Retrofit = Retrofit.Builder().baseUrl(Api.BASE_URL)
            .addConverterFactory(GsonConverterFactory.create())
            .client(getOkHttpClientBuilder().build())
            .build()
        myApi = retrofit.create(Api::class.java)
    }

    fun getMyApi(): Api {
        return myApi
    }


    private fun getOkHttpClientBuilder(): OkHttpClient.Builder {
         val interceptorProvider = InterceptorProvider()
        return OkHttpClient.Builder()
            .connectTimeout(100, TimeUnit.SECONDS)
            .readTimeout(100, TimeUnit.SECONDS)
            .apply {
                for (interceptor in interceptorProvider.interceptors) {
                    addInterceptor(interceptor)
                }
            }
    }
    companion object {
        @get:Synchronized
        var instance: RetrofitClient? = null
            get() {
                if (field == null) {
                    field = RetrofitClient()
                }
                return field
            }
            private set
    }
}