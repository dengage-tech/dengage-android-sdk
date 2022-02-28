package com.dengage.sdk.callback

interface DengageCallback<T> {

    fun onResult(result: T)
    fun onError(error: DengageError)

}