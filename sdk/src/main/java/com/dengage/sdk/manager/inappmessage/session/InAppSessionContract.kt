package com.dengage.sdk.manager.inappmessage.session

import com.dengage.sdk.manager.base.BasePresenter
import com.dengage.sdk.manager.base.BaseView

interface InAppSessionContract {

    interface View : BaseView

    interface Presenter : BasePresenter<View> {
        fun setLastSessionStartTime()
        fun setLastSessionDuration()
        fun setLastVisitTime()
        fun sendAppForegroundEvent()
        fun sendFirstLaunchEvent()
    }

}