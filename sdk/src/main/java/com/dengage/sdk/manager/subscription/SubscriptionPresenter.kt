package com.dengage.sdk.manager.subscription

import android.os.Handler
import android.os.Looper
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.subscription.usecase.SendSubscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch

class SubscriptionPresenter : BaseAbstractPresenter<SubscriptionContract.View>(),
    SubscriptionContract.Presenter {

    private val sendSubscription by lazy { SendSubscription() }
    private var sendSubscriptionTryCount = 0
    private var subscriptionInProgress =false

    override fun sendSubscription(subscription: Subscription) {
        Handler(Looper.getMainLooper()).postDelayed({
            if (DengageUtils.isAppInForeground()&&!subscriptionInProgress) {
                if (Prefs.subscription != Prefs.previouSubscription) {
                    Prefs.subscription?.let { callSubscriptionApi(it) }
                } else if (System.currentTimeMillis() > Prefs.subscriptionCallTime) {
                    Prefs.subscription?.let { callSubscriptionApi(it) }
                }
            }
        }, 4000)
    }


    private fun callSubscriptionApi(subscription: Subscription) {
        subscriptionInProgress=true
        DengageLogger.verbose("sub method is called")
        sendSubscriptionTryCount++
        sendSubscription(this) {
            onResponse = {
                view {
                    subscriptionInProgress=false
                    Prefs.previouSubscription = Prefs.subscription
                    Prefs.subscriptionCallTime = System.currentTimeMillis() + (20 * 60 * 1000)

                    subscriptionSent() }
            }
            onError = {
                // try to send it for 5 times
                subscriptionInProgress=false
                if (sendSubscriptionTryCount < 5) {
                    sendSubscription(
                        subscription = subscription
                    )
                } else {
                    sendSubscriptionTryCount = 0
                }
            }
            params = SendSubscription.Params(
                subscription = subscription
            )
        }
    }
}
