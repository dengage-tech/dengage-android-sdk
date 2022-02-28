package com.dengage.sdk.manager.base

interface BaseView {
    fun showLoading()
    fun hideLoading()
    fun showError(error: Throwable)
}