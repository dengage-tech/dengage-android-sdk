package com.dengage.sdk.manager.inappmessage.session

import com.dengage.sdk.manager.base.BaseMvpManager

class InAppSessionManager : BaseMvpManager<InAppSessionContract.View,
    InAppSessionContract.Presenter>(),
    InAppSessionContract.View {

    override fun providePresenter() = InAppSessionPresenter()

    internal fun setLastSessionStartTime() {
        presenter.setLastSessionStartTime()
    }

    internal fun setLastSessionDuration() {
        presenter.setLastSessionDuration()
    }

    internal fun setLastVisitTime() {
        presenter.setLastVisitTime()
    }

    internal fun sendAppForegroundEvent() {
        presenter.sendAppForegroundEvent()
    }

    internal fun sendFirstLaunchEvent() {
        presenter.sendFirstLaunchEvent()
    }

}
