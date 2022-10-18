package com.dengage.sdk.manager.deviceId


import com.dengage.sdk.manager.base.BasePresenter
import com.dengage.sdk.manager.base.BaseView

class DeviceIdSenderContract {

    interface View : BaseView {
        fun deviceIdSent()
    }

    interface Presenter : BasePresenter<View> {
        fun sendDeviceId(
            route: String,
            token:String,
            deviceId:String

        )
    }
}