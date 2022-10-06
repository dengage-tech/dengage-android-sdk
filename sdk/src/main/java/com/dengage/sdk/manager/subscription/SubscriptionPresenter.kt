package com.dengage.sdk.manager.subscription

import android.os.Handler
import android.os.Looper
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.subscription.usecase.SendSubscription
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger

class SubscriptionPresenter : BaseAbstractPresenter<SubscriptionContract.View>(),
    SubscriptionContract.Presenter {

    private val sendSubscription by lazy { SendSubscription() }

    private var sendSubscriptionTryCount = 0

    override fun sendSubscription(subscription: Subscription) {
        Handler(Looper.getMainLooper()).postDelayed({
            if (Constants.sendSubscription) {
                DengageLogger.verbose("sub method is called")
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
        }, 4000)

    }

}
