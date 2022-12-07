package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class InAppRemovalId(
    @SerializedName("smsg_id") val id: String,
) : Serializable
