package com.dengage.sdk.domain.inboxchannel.model

import com.google.gson.annotations.SerializedName

/**
 * Event types accepted by the Inbox Channel /inbox/events endpoint.
 * The short code is what gets sent to the server.
 */
enum class InboxChannelEventType(val code: String) {
    @SerializedName("IM") IMPRESSION("IM"),
    @SerializedName("OP") OPEN("OP"),
    @SerializedName("CL") CLICK("CL"),
    @SerializedName("DT") DELETE("DT")
}
