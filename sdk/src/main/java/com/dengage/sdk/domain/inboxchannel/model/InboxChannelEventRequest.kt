package com.dengage.sdk.domain.inboxchannel.model

import com.google.gson.annotations.SerializedName

/**
 * Request body for POST /api/inbox/events. Supports bulk sending: one request
 * can carry multiple events.
 */
data class InboxChannelEventRequest(
    @SerializedName("acc") val account: String,
    @SerializedName("ckey") val contactKey: String?,
    @SerializedName("did") val deviceId: String,
    @SerializedName("appId") val appId: String?,
    @SerializedName("events") val events: List<InboxChannelEventItem>
)

data class InboxChannelEventItem(
    @SerializedName("eventType") val eventType: InboxChannelEventType,
    @SerializedName("msgid") val msgId: String,
    @SerializedName("messageDetails") val messageDetails: String?,
    /** Always UTC now, stamped by the SDK when the request is built. */
    @SerializedName("eventDateUTC") val eventDateUTC: String
)
