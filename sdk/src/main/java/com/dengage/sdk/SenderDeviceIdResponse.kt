package com.dengage.sdk

import com.dengage.sdk.domain.inappmessage.model.InAppMessageData
import com.google.gson.annotations.SerializedName
import java.io.Serializable

class SenderDeviceIdResponse(
@SerializedName("sessionDeviceId") val deviceId: String,
) : Serializable
