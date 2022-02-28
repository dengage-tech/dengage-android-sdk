package com.dengage.sdk.data.remote.provider


class UrlProvider(private val baseUrl: String?) {

    // todo update url
    val url: String = baseUrl!! /*?: BuildConfig.API_BASE_URL*/
}
