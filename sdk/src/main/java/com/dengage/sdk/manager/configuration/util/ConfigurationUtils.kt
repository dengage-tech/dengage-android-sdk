package com.dengage.sdk.manager.configuration.util

import com.dengage.sdk.push.IDengageHmsManager
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.google.android.gms.ads.identifier.AdvertisingIdClient
import com.google.android.gms.common.ConnectionResult
import com.google.android.gms.common.GoogleApiAvailabilityLight
import com.google.firebase.FirebaseApp
import com.google.firebase.messaging.FirebaseMessaging
import java.util.concurrent.Executors

object ConfigurationUtils {

    var dengageHmsManager: IDengageHmsManager? = null

    fun isGooglePlayServicesAvailable(): Boolean {
        return try {
            GoogleApiAvailabilityLight.getInstance().isGooglePlayServicesAvailable(ContextHolder.context) == ConnectionResult.SUCCESS
        } catch (ignored: java.lang.Exception) {
            false
        }
    }

    fun isHuaweiMobileServicesAvailable(): Boolean {
        return try {
            dengageHmsManager?.isHuaweiMobileServicesAvailable() ?: false
        } catch (ignored: java.lang.Exception) {
            false
        }
    }

    fun getFirebaseToken(firebaseApp: FirebaseApp?, onTokenResult: (String) -> Unit) {
        try {
            DengageLogger.debug("Getting Firebase Token")
            val executor = Executors.newSingleThreadExecutor()
            executor.execute {


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
        }
        catch (_:Exception){}
        catch (_:Throwable){}
    }

    fun getHuaweiToken(onTokenResult: (String) -> Unit) {
        dengageHmsManager?.getHuaweiToken(onTokenResult)
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
        dengageHmsManager?.getHmsAdvertisingId(onAdIdResult)
    }
}