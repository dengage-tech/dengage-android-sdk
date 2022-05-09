package com.dengage.sdk.manager.configuration.util

import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.google.android.gms.ads.identifier.AdvertisingIdClient
import com.google.android.gms.common.ConnectionResult
import com.google.android.gms.common.GoogleApiAvailabilityLight
import com.google.firebase.FirebaseApp
import com.google.firebase.messaging.FirebaseMessaging
import com.huawei.agconnect.AGConnectOptionsBuilder
import com.huawei.hms.aaid.HmsInstanceId
import com.huawei.hms.api.HuaweiApiAvailability
import com.huawei.hms.common.ApiException

object ConfigurationUtils {

    fun isGooglePlayServicesAvailable(): Boolean {
        return try {
            GoogleApiAvailabilityLight.getInstance().isGooglePlayServicesAvailable(ContextHolder.context) == ConnectionResult.SUCCESS
        } catch (ignored: java.lang.Exception) {
            false
        }
    }

    fun isHuaweiMobileServicesAvailable(): Boolean {
        return try {
            HuaweiApiAvailability.getInstance().isHuaweiMobileServicesAvailable(ContextHolder.context) == com.huawei.hms.api.ConnectionResult.SUCCESS
        } catch (ignored: java.lang.Exception) {
            false
        }
    }

    fun getFirebaseToken(firebaseApp: FirebaseApp?, onTokenResult: (String) -> Unit) {
        DengageLogger.debug("Getting Firebase Token")
        val firebaseMessaging: FirebaseMessaging = if (firebaseApp == null) {
            FirebaseMessaging.getInstance()
        } else {
            firebaseApp[FirebaseMessaging::class.java] ?: FirebaseMessaging.getInstance()
        }

        firebaseMessaging.token.addOnCompleteListener { task ->
            if (task.isSuccessful) {
                val token = task.result
                if (token == null) {
                    DengageLogger.error("Firebase token is null")
                } else {
                    DengageLogger.debug("Firebase token is $token")
                    onTokenResult.invoke(token)
                }
            } else {
                DengageLogger.error("Firebase InstanceId Failed: " + task.exception?.message)
            }
        }
    }

    fun getHuaweiToken(onTokenResult: (String) -> Unit) {
        DengageLogger.debug("Getting Hms Token")
        object : Thread() {
            override fun run() {
                try {
                    // Set tokenScope to HCM.
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

    fun getGmsAdvertisingId(onAdIdResult: (String) -> Unit) {
        DengageLogger.debug("Getting GMS advertising ID")
        object : Thread() {
            override fun run() {
                try {
                    val advertisingIdInfo = AdvertisingIdClient.getAdvertisingIdInfo(
                        ContextHolder.context
                    )
                    if (!advertisingIdInfo.isLimitAdTrackingEnabled) {
                        val advertisingId = advertisingIdInfo.id
                        if (!advertisingId.isNullOrEmpty()) {
                            onAdIdResult.invoke(advertisingId)
                        }
                    }
                } catch (e: Exception) {
                    DengageLogger.error("GmsAdIdWorker Exception: ${e.message}")
                }
            }
        }.start()
    }

    fun getHmsAdvertisingId(onAdIdResult: (String) -> Unit) {
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