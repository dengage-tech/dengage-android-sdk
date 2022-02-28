package com.dengage.sdk.domain.event.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class OpenEvent(
    @SerializedName("buttonId") var buttonId: String?,
    @SerializedName("itemId") var itemId: String?,
    @SerializedName("messageId") var messageId: Int?,
    @SerializedName("messageDetails") var messageDetails: String?,
    @SerializedName("userAgent") var userAgent: String? = "",
    @SerializedName("integrationKey") var integrationKey: String? = ""
) : Serializable