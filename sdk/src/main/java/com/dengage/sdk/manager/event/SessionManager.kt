package com.dengage.sdk.manager.event

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.util.DengageUtils
import java.util.*

object SessionManager {

    private fun isExpired(): Boolean {
        return Calendar.getInstance().time > Date(Prefs.appSessionTime)
    }

    fun getSessionId(): String {
        var sessionId = Prefs.appSessionId
        if (isExpired()) {
            sessionId = DengageUtils.generateUUID()
            Prefs.appSessionId = sessionId

            val expireCalendar = Calendar.getInstance()
            expireCalendar.add(Calendar.MINUTE, 30)
            Prefs.appSessionTime = expireCalendar.time.time
        }
        return sessionId
    }

}