package com.dengage.sdk.manager.subscription

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.subscription.usecase.SendSubscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.delay
import kotlinx.coroutines.launch

class SubscriptionPresenter : BaseAbstractPresenter<SubscriptionContract.View>(),
    SubscriptionContract.Presenter {

    private val sendSubscription by lazy { SendSubscription() }
    private val scope = CoroutineScope(Dispatchers.IO)

    private var sendSubscriptionTryCount = 0

    override fun sendSubscription(subscription: Subscription) {
        scope.launch {
            delay(4000)
            if (DengageUtils.foregrounded()) {
                if (Prefs.subscription != Prefs.previouSubscription) {
                    Prefs.subscription?.let { callSubscriptionApi(it) }
                } else if (System.currentTimeMillis() > Prefs.subscriptionCallTime) {
                    Prefs.subscription?.let { callSubscriptionApi(it) }
                }
            }

        }
    }


    private fun callSubscriptionApi(subscription: Subscription) {
        DengageLogger.verbose("sub method is called")
        sendSubscriptionTryCount++
        sendSubscription(this) {
            onResponse = {
                view {
                    Prefs.previouSubscription = Prefs.subscription
                    Prefs.subscriptionCallTime = System.currentTimeMillis() + (20 * 60 * 1000)

                    subscriptionSent() }
            }
            onError = {
                // try to send it for 5 times
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
