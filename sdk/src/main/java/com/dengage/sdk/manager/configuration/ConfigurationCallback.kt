package com.dengage.sdk.manager.configuration

import com.dengage.sdk.domain.configuration.model.AppTracking
import com.dengage.sdk.domain.subscription.model.Subscription

interface ConfigurationCallback {

    fun sendSubscription(subscription: Subscription)
    fun fetchInAppMessages()
    fun startAppTracking(appTrackings: List<AppTracking>?)

}
