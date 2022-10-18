package com.dengage.sdk.manager.deviceId

import com.dengage.sdk.domain.deviceId.usecase.DeviceIdSenderToServer
import com.dengage.sdk.manager.base.BaseAbstractPresenter

class DeviceIdSenderPresenter  : BaseAbstractPresenter<DeviceIdSenderContract.View>(),
    DeviceIdSenderContract.Presenter {

    private val sendDeviceId by lazy { DeviceIdSenderToServer() }

    override fun sendDeviceId(route: String?,token :String?,deviceId:String) {
        sendDeviceId(this) {
            onResponse = {
                view { deviceIdSent() }
            }
            params = DeviceIdSenderToServer.Params(
                route = route,
                token = token,
                deviceId=deviceId

            )
        }
    }
}
