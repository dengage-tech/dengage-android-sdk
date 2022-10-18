package com.dengage.sdk.manager.deviceId

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.manager.base.BaseMvpManager

class DeviceIdSenderManager : BaseMvpManager<DeviceIdSenderContract.View, DeviceIdSenderContract.Presenter>(),
    DeviceIdSenderContract.View {

    override fun providePresenter() = DeviceIdSenderPresenter()

    fun sendDeviceId(
        route: String?,
        token: String?,
    ) {
        Prefs.subscription.let {
            if (it != null) {
                presenter.sendDeviceId(route,token, it.getSafeDeviceId())
            }
        }
    }

    override fun deviceIdSent()  = Unit

}