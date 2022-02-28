package com.dengage.sdk.manager.base

import androidx.annotation.UiThread

interface BasePresenter<View : BaseView> {

    @UiThread
    fun attachView(view: BaseView)

    @UiThread
    fun detachView()

    /**
     * Gets the attached view if it is attached.
     *
     * @return the view if it is attached, otherwise null
     */
    @UiThread
    fun getViewIfIsAttached(): View?

    /**
     * Gets the attached view. You should call [isViewAttached] to avoid exceptions.
     *
     * @return the view if it is attached
     * @throws [IllegalArgumentException] if no view is attached
     */
    @UiThread
    fun getView(): View

    /**
     * Checks if a view is attached to this presenter.
     *
     * @return false if no view is attached
     */
    @UiThread
    fun isViewAttached(): Boolean
}