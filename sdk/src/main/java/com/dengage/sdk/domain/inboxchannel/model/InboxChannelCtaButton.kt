package com.dengage.sdk.domain.inboxchannel.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class InboxChannelCtaButton(
    @SerializedName("buttonId") val buttonId: String?,
    @SerializedName("label") val label: String?,
    @SerializedName("iosDeeplink") val iosDeeplink: String?,
    @SerializedName("androidDeeplink") val androidDeeplink: String?,
    @SerializedName("webUrl") val webUrl: String?
) : Serializable
