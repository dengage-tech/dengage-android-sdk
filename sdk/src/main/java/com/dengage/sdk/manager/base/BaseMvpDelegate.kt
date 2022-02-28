package com.dengage.sdk.manager.base

import androidx.annotation.CallSuper

interface BaseMvpDelegate<View : BaseView, Presenter : BasePresenter<View>> {
    var _view: View?
    var _presenter: Presenter?

    /**
     * Call this function when the [View] is creating
     */
    @CallSuper
    fun attach() {
        _getPresenter().attachView(getMvpView())
        onPresenterAttached()
    }

    /**
     * Call this function when the [View] is destroying
     */
    @CallSuper
    fun detach() {
        onPresenterDetached()
        _getPresenter().detachView()
    }

    /**
     * Get the [ProtelView] for the presenter.
     *
     * @return The view associated with the presenter
     */
    fun getMvpView(): View {
        if (_view == null) {
            @Suppress("UNCHECKED_CAST")
            _view = this as View
        }
        return _view!!
    }

    /**
     * Create a presenter instance
     *
     * @return the created presenter instance
     */
    fun providePresenter(): Presenter? = null

    /**
     * Set the presenter instance
     *
     * @param presenter The presenter instance
     */
    fun setPresenter(presenter: Presenter) {
        _presenter = presenter
    }

    /**
     * Get the presenter. If it is null, then a internally a new presenter instance gets
     * created by calling [providePresenter]
     *
     * @return the presenter instance
     */
    fun _getPresenter(): Presenter {
        if (_presenter == null) {
            _presenter = providePresenter()
        }
        return _presenter!!
    }

    /**
     * Override this on child view if needed
     */
    fun onPresenterAttached() = Unit

    /**
     * Override this on child view if needed
     */
    fun onPresenterDetached() = Unit
}