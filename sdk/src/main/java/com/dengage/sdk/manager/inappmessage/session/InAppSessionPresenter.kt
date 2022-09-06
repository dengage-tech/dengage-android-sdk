package com.dengage.sdk.manager.inappmessage.session

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.usecase.SendAppForegroundEvent
import com.dengage.sdk.domain.inappmessage.usecase.SendFirstLaunchEvent
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.manager.session.SessionManager
import java.util.concurrent.TimeUnit

class InAppSessionPresenter : BaseAbstractPresenter<InAppSessionContract.View>(),
    InAppSessionContract.Presenter {

    private val sendAppForegroundEvent by lazy { SendAppForegroundEvent() }
    private val sendFirstLaunchEvent by lazy { SendFirstLaunchEvent() }

    override fun setLastSessionStartTime() {
        Prefs.lastSessionStartTime = TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis())
    }

    override fun setLastSessionDuration() {
        val lastSessionStartTime = Prefs.lastSessionStartTime
        if (lastSessionStartTime != 0L) {
            Prefs.lastSessionDuration = TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis()) - lastSessionStartTime
        }
    }

    override fun setLastVisitTime() {
        Prefs.lastSessionVisitTime = TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis())
    }

    override fun sendAppForegroundEvent() {
        val sdkParameters = Prefs.sdkParameters
        val subscription = Prefs.subscription
        val lastSessionDuration = Prefs.lastSessionDuration
        if (subscription != null && sdkParameters != null && lastSessionDuration != 0L) {
            sendAppForegroundEvent(this) {
                onResponse = {
                    Prefs.lastSessionDuration = 0
                }
                params = SendAppForegroundEvent.Params(
                    accountName = sdkParameters.accountName,
                    subscription = subscription,
                    appId = sdkParameters.appId,
                    sessionId = SessionManager.getSessionId(),
                    duration = Prefs.lastSessionDuration
                )
            }
        }
    }

    override fun sendFirstLaunchEvent() {
        if (Prefs.firstLaunchTime != 0L) return

        val sdkParameters = Prefs.sdkParameters
        val subscription = Prefs.subscription
        if (subscription != null && sdkParameters != null) {
            sendFirstLaunchEvent(this) {
                onResponse = {
                    Prefs.firstLaunchTime = TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis())
                }
                params = SendFirstLaunchEvent.Params(
                    accountName = sdkParameters.accountName,
                    subscription = subscription,
                    appId = sdkParameters.appId,
                    sessionId = SessionManager.getSessionId()
                )
            }
        }
    }

}
