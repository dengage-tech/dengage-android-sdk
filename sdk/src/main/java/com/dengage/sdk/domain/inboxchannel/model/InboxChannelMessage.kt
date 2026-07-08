package com.dengage.sdk.domain.inboxchannel.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

/**
 * A single Inbox Channel message returned by /api/inbox/getMessages.
 */
data class InboxChannelMessage(
    @SerializedName("smsgId") val id: String,
    @SerializedName("isRead") var isRead: Boolean = false,
    @SerializedName("priority") val priority: Int = 0,
    @SerializedName("messageJson") val data: InboxChannelMessageData
) : Serializable {
    /** Local-only flag; not returned by the server. Reflects a pending delete. */
    var isDeleted: Boolean = false
}
