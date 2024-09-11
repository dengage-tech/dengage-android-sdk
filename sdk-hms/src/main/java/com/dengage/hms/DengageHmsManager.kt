package com.dengage.hms

import com.dengage.sdk.push.IDengageHmsManager
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.huawei.hms.api.HuaweiApiAvailability
import com.huawei.agconnect.AGConnectOptionsBuilder
import com.huawei.hms.aaid.HmsInstanceId

class DengageHmsManager : IDengageHmsManager {

    override fun isHuaweiMobileServicesAvailable(): Boolean {
        return try {
            HuaweiApiAvailability.getInstance()
                .isHuaweiMobileServicesAvailable(ContextHolder.context) == com.huawei.hms.api.ConnectionResult.SUCCESS
        } catch (ignored: java.lang.Exception) {
            false
        }
    }

    override fun getHuaweiToken(onTokenResult: (String) -> Unit) {
        DengageLogger.debug("Getting Hms Token")
        object : Thread() {
            override fun run() {
                try {
                    val tokenScope = "HCM"
                    val huaweiAppId = AGConnectOptionsBuilder().build(ContextHolder.context).getString("client/app_id")
                    val token = HmsInstanceId.getInstance(ContextHolder.context).getToken(huaweiAppId, tokenScope)

                    if (token == null) {
                        DengageLogger.error("Hms token is null")
                    } else {
                        DengageLogger.debug("Hms appId is $huaweiAppId, token is $token")
                        onTokenResult.invoke(token)
                    }
                } catch (e: Exception) {
                    DengageLogger.error("Hms InstanceId Failed: ${e.message}")
                }
            }
        }.start()
    }

    override fun getHmsAdvertisingId(onAdIdResult: (String) -> Unit) {
        DengageLogger.debug("Getting HMS advertising ID")
        object : Thread() {
            override fun run() {
                try {
                    val advertisingIdInfo = com.huawei.hms.ads.identifier.AdvertisingIdClient.getAdvertisingIdInfo(
                        ContextHolder.context
                    )
                    if (!advertisingIdInfo.isLimitAdTrackingEnabled) {
                        val advertisingId = advertisingIdInfo.id
                        if (!advertisingId.isNullOrEmpty()) {
                            onAdIdResult.invoke(advertisingId)
                        }
                    }
                } catch (e: Exception) {
                    DengageLogger.error("HmsAdIdWorker Exception: ${e.message}")
                }
            }
        }.start()
    }
}