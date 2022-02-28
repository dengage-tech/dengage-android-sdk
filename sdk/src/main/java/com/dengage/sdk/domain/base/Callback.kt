package com.dengage.sdk.domain.base

open class Callback<T>(
    private val onStart: (() -> Unit)? = null,
    private val onComplete: (() -> Unit)? = null,
    private val onError: ((Throwable) -> Unit)? = null,
    private val onResponse: ((T) -> Unit)? = null
) {
    open fun onStart() {
        onStart?.invoke()
    }

    open fun onResponse(response: T) {
        onResponse?.invoke(response)
    }

    open fun onError(error: Throwable) {
        onError?.invoke(error)
    }

    open fun onComplete() {
        onComplete?.invoke()
    }
}