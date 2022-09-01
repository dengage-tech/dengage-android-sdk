package com.dengage.sdk.util.extension

import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.util.GsonHolder

fun Subscription.getCdKey(): String {
    return if (contactKey.isNullOrEmpty()) {
        getSafeDeviceId()
    } else {
        contactKey!!
    }
}

fun Subscription.getType(): String {
    return if (contactKey.isNullOrEmpty()) {
        "d"
    } else {
        "c"
    }
}

fun SdkParameters.getAppId(): String {
    return if (appId.isNullOrEmpty()) {
        ""
    } else {
        appId.toString()
    }
}

fun Any?.toJson(): String {
    return GsonHolder.gson.toJson(this)
}