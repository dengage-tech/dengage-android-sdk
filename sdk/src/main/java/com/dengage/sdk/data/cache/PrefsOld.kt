package com.dengage.sdk.data.cache

import android.content.Context
import android.content.SharedPreferences
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.ContextHolder

@Deprecated("Use Prefs.kt class after first release of new sdk")
object PrefsOld {

    private val preferences: SharedPreferences by lazy {
        ContextHolder.context.getSharedPreferences(ContextHolder.context.packageName, Context.MODE_PRIVATE)
    }

    var subscription: Subscription?
        get() = preferences.get("dengage_subscription")
        set(value) = preferences.set("dengage_subscription", value)
}