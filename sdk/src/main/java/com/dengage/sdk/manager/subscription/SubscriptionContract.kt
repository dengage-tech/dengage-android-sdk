package com.dengage.sdk.manager.subscription

import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.manager.base.BasePresenter
import com.dengage.sdk.manager.base.BaseView

interface SubscriptionContract {

    interface View : BaseView {
        fun subscriptionSent()
    }

    interface Presenter : BasePresenter<View> {
        fun enqueueSubscription(subscription: Subscription)
        fun callSubscriptionApi(subscription: Subscription)
    }
}