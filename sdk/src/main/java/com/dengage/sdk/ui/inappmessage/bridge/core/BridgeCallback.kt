package com.dengage.sdk.ui.inappmessage.bridge.core

/**
 * Callback interface for async bridge operations
 */
interface BridgeCallback<T> {
    fun onSuccess(data: T?)
    fun onError(errorCode: String, errorMessage: String)
}
