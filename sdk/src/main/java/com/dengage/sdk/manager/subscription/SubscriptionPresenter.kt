package com.dengage.sdk.manager.subscription

import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.subscription.usecase.SendSubscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter

class SubscriptionPresenter : BaseAbstractPresenter<SubscriptionContract.View>(),
    SubscriptionContract.Presenter {

    private val sendSubscription by lazy { SendSubscription() }

    private var sendSubscriptionTryCount = 0

    override fun sendSubscription(subscription: Subscription) {
        sendSubscriptionTryCount++
        sendSubscription(this) {
            onResponse = {
                view { subscriptionSent() }
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
