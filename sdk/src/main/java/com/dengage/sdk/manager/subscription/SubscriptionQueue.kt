package com.dengage.sdk.manager.subscription

import android.os.Handler
import android.os.Looper
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.util.DengageLogger

class SubscriptionQueue(
    private val presenter: SubscriptionContract.Presenter
) {
    private var subscriptionRequestWorkItem: Runnable? = null
    private val handler = Handler(Looper.getMainLooper())

    private val subscriptionRequestDelay: Long = 5000

    fun enqueueSubscription(subscription: Subscription) {
        subscriptionRequestWorkItem?.let { handler.removeCallbacks(it) }

        subscriptionRequestWorkItem = Runnable {
            DengageLogger.verbose("SubscriptionQueue -> performSubscriptionRequest")
            presenter.callSubscriptionApi(subscription)
        }

        handler.postDelayed(subscriptionRequestWorkItem!!, subscriptionRequestDelay)
    }

    fun cancelPendingRequest() {
        subscriptionRequestWorkItem?.let {
            handler.removeCallbacks(it)
            subscriptionRequestWorkItem = null
        }
    }
}
