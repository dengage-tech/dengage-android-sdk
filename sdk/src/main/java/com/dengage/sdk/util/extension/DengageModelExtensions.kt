package com.dengage.sdk.util.extension

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.push.model.Message
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
    try{
    return GsonHolder.gson.toJson(this)

} catch (ex: Exception)
{ ex.printStackTrace()

}
catch (ex: Throwable)
{ ex.printStackTrace()

}
catch (ex :IncompatibleClassChangeError)
{ ex.printStackTrace()

}
catch (ex :NoSuchFieldError)
{ ex.printStackTrace()

}
catch (ex :NoSuchMethodError)
{ ex.printStackTrace()

}
catch (ex:java.lang.AssertionError)
{
    ex.printStackTrace()
}
catch (ex:AssertionError)
{
    ex.printStackTrace()
}
return ""
}

fun Message.storeToPref() {
    try {
        Prefs.lastPushPayload = this
    }
    catch (e:Exception){}
}
