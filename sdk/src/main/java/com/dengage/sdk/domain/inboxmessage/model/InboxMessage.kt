package com.dengage.sdk.domain.inboxmessage.model

import com.google.gson.annotations.JsonAdapter
import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class InboxMessage(
    @SerializedName("smsg_id") val id: String,
    @SerializedName("is_clicked") var isClicked: Boolean,
    @SerializedName("message_json")
    val data: InboxMessageData
) : Serializable
