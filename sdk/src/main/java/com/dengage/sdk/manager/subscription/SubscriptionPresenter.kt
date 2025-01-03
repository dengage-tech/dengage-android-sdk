package com.dengage.sdk.manager.subscription

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.subscription.usecase.SendSubscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter

class SubscriptionPresenter : BaseAbstractPresenter<SubscriptionContract.View>(),
    SubscriptionContract.Presenter {

    private val sendSubscription by lazy { SendSubscription() }
    private var sendSubscriptionTryCount = 0
    private var subscriptionInProgress =false

    private val subscriptionQueue: SubscriptionQueue by lazy {
        SubscriptionQueue(this)
    }

    override fun sendSubscription(subscription: Subscription) {
        subscriptionQueue.enqueueSubscription(subscription)
    }

    /*
    override fun sendSubscription(subscription: Subscription) {
        Handler(Looper.getMainLooper()).postDelayed({
            if (DengageUtils.isAppInForeground()/*&&!subscriptionInProgress*/) {
                if (Prefs.subscription != Prefs.previouSubscription) {
                    Prefs.subscription?.let { callSubscriptionApi(it) }
                } else if (System.currentTimeMillis() > Prefs.subscriptionCallTime) {
                    Prefs.subscription?.let { callSubscriptionApi(it) }
                }
            }
        }, 4000)
    }
    */

    override fun callSubscriptionApi(subscription: Subscription) {
        subscriptionInProgress = true
        sendSubscriptionTryCount++

        sendSubscription(this) {
            onResponse = {
                view {
                    subscriptionInProgress = false
                    Prefs.previouSubscription = Prefs.subscription
                    Prefs.subscriptionCallTime = System.currentTimeMillis() + (20 * 60 * 1000)
                    subscriptionSent()
                }
            }
            onError = {
                subscriptionInProgress = false
                if (sendSubscriptionTryCount < 5) {
                    callSubscriptionApi(subscription)
                } else {
                    sendSubscriptionTryCount = 0
                }
            }
            params = SendSubscription.Params(subscription)
        }
    }
}
