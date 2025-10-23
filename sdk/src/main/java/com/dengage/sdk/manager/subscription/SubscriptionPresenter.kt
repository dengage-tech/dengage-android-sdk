package com.dengage.sdk.manager.subscription

import android.os.Handler
import android.os.Looper
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.subscription.usecase.SendSubscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils

class SubscriptionPresenter : BaseAbstractPresenter<SubscriptionContract.View>(),
    SubscriptionContract.Presenter {

    private val sendSubscriptionUseCase by lazy { SendSubscription() }
    private var sendSubscriptionTryCount = 0
    private val handler = Handler(Looper.getMainLooper())
    private var currentWorkItem: Runnable? = null

    override fun enqueueSubscription(subscription: Subscription) {
        currentWorkItem?.let {
            handler.removeCallbacks(it)
        }
        currentWorkItem = Runnable {
            DengageLogger.verbose("SubscriptionPresenter -> Checking if we should call subscription API now.")
            if (shouldCallSubscriptionApi(subscription)) {
                DengageLogger.verbose("SubscriptionPresenter -> Conditions met. Now calling callSubscriptionApi.")
                callSubscriptionApi(subscription)
            }
        }.also { task ->
            handler.postDelayed(task, QUEUE_DELAY)
        }
    }

    override fun callSubscriptionApi(subscription: Subscription) {
        sendSubscriptionTryCount++
        sendSubscriptionUseCase(this) {
            onResponse = {
                view {
                    Prefs.previousSubscription = Prefs.subscription
                    Prefs.subscriptionCallTime = System.currentTimeMillis() + (TWENTY_MINUTES)
                    subscriptionSent()
                }
            }
            onError = {
                // Retry up to 5 times
                if (sendSubscriptionTryCount < 5) {
                    callSubscriptionApi(subscription)
                } else {
                    sendSubscriptionTryCount = 0
                }
            }
            params = SendSubscription.Params(subscription)
        }
    }

    private fun shouldCallSubscriptionApi(subscription: Subscription): Boolean {
        if(!DengageUtils.isAppInForeground()) return false
        val now = System.currentTimeMillis()
        val nextAllowedTime = Prefs.subscriptionCallTime
        val prevSubscription = Prefs.previousSubscription
        val subscriptionChanged = (subscription != prevSubscription)
        val timePassed = now >= nextAllowedTime
        return (subscriptionChanged || timePassed)
    }

    companion object {
        private const val QUEUE_DELAY = 5000L // 5 seconds
        private const val TWENTY_MINUTES = 20 * 60 * 1000
    }
}
