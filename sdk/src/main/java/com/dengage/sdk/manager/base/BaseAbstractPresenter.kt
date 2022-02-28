package com.dengage.sdk.manager.base

import androidx.annotation.CallSuper
import com.dengage.sdk.domain.base.Callback
import com.dengage.sdk.domain.base.UseCaseRunner
import com.dengage.sdk.domain.base.UseCaseRunnerDelegate
import java.lang.ref.WeakReference

abstract class BaseAbstractPresenter<View : BaseView>
    : BasePresenter<View>,
    UseCaseRunner by UseCaseRunnerDelegate() {

    private var viewRef: WeakReference<View>? = null

    @CallSuper
    override fun attachView(view: BaseView) {
        @Suppress("UNCHECKED_CAST")
        viewRef = WeakReference(view as View)
        onViewAttached()
    }

    @CallSuper
    override fun detachView() {
        viewRef?.clear()
        viewRef = null
        onViewDetached()
    }

    override fun getViewIfIsAttached(): View? {
        return viewRef?.get()
    }

    override fun getView(): View {
        return getViewIfIsAttached() ?: throw IllegalArgumentException("View is detached")
    }

    override fun isViewAttached(): Boolean {
        return getViewIfIsAttached() != null
    }

    protected fun view(block: View.() -> Unit) {
        getViewIfIsAttached()?.apply(block)
    }

    protected open fun onViewAttached() {
        // Override this on child fragments if needed.
    }

    fun onViewDetached() {
        cancelUseCases()
    }

    override fun <T> callback(
        onStart: (() -> Unit)?,
        onResponse: ((T) -> Unit)?,
        onError: ((Throwable) -> Unit)?,
        onComplete: (() -> Unit)?
    ): Callback<T> =
        Callback(
            onStart = {
                view { showLoading() }
                onStart?.invoke()
            },
            onResponse = {
                if (onResponse != null) {
                    onResponse(it)
                }
            },
            onError = {
                if (onError != null) {
                    onError(it)
                } else {
                    view { showError(it) }
                }
            },
            onComplete = {
                view { hideLoading() }
                onComplete?.invoke()
            }
        )
}