package com.dengage.sdk.push

interface IDengageHmsManager {
    fun isHuaweiMobileServicesAvailable(): Boolean
    fun getHuaweiToken(onTokenResult: (String) -> Unit)
    fun getHmsAdvertisingId(onAdIdResult: (String) -> Unit)
}