package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class InAppCancelledSendId(
    @SerializedName("send_id") val sendId: Int,
) : Serializable
