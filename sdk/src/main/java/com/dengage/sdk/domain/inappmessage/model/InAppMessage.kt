package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class InAppMessage(
    @SerializedName("smsg_id") val id: String,
    @SerializedName("message_json") val data: InAppMessageData
) : Serializable
