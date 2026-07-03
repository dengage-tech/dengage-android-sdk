package com.dengage.sdk.domain.inboxchannel.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

/**
 * Content payload of an Inbox Channel message (the "messageJson" object).
 */
data class InboxChannelMessageData(
    @SerializedName("title") val title: String?,
    @SerializedName("message") val message: String?,
    @SerializedName("imageUrl") val imageUrl: String?,
    @SerializedName("ctaButtons") val ctaButtons: List<InboxChannelCtaButton>?,
    @SerializedName("isPinned") val isPinned: Boolean = false,
    @SerializedName("receiveDateUTC") val receiveDate: String?,
    /**
     * Opaque token returned by getMessages that must be echoed back
     * in the messageDetails field of every /inbox/events request.
     */
    @SerializedName("messageDetails") val messageDetails: String?
) : Serializable
