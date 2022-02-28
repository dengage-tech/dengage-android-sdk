package com.dengage.sdk.manager.base

import com.dengage.sdk.data.remote.error.DengageApiError
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.extension.mapToDengageApiError

abstract class BaseMvpManager<V : BaseView, P : BasePresenter<V>> : BaseView,
    BaseMvpDelegate<V, P> {

    override var _view: V? = null
    override var _presenter: P? = null

    protected val presenter: P
        get() = _getPresenter()

    init {
        onCreate()
    }

    fun onCreate() {
        attach()
    }

    fun onDestroy() {
        detach()
    }

    override fun showLoading() = Unit

    override fun hideLoading() = Unit

    override fun showError(error: Throwable) {
        showError(error.mapToDengageApiError())
    }

    open fun showError(dengageApiError: DengageApiError) {
        hideLoading()
    }
}