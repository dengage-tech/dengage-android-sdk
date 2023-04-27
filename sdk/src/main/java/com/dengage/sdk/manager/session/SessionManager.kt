package com.dengage.sdk.manager.session

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.manager.inappmessage.util.RealTimeInAppParamHolder
import com.dengage.sdk.manager.visitcount.VisitCountManager
import com.dengage.sdk.util.DengageUtils
import java.util.*

object SessionManager {

    private fun isExpired(): Boolean {
        return Calendar.getInstance().time > Date(Prefs.appSessionTime)
    }

    fun getSessionId(force: Boolean = false): String {
        try {
            var sessionId = Prefs.appSessionId
            if (force || isExpired()) {
                sessionId = DengageUtils.generateUUID()
                Prefs.appSessionId = sessionId

                val expireCalendar = Calendar.getInstance()
                expireCalendar.add(
                    Calendar.MINUTE,
                    Prefs.sdkParameters?.realTimeInAppSessionTimeoutMinutes ?: 30
                )
                Prefs.appSessionTime = expireCalendar.time.time

                VisitCountManager.updateVisitCount()
                RealTimeInAppParamHolder.pageViewVisitCount = 0
            }
            return sessionId
        }
        catch (e:Exception){
            return ""
        }
    }

}