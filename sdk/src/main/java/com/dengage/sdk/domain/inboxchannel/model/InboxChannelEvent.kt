package com.dengage.sdk.domain.inboxchannel.model

import java.io.Serializable

/**
 * A single Inbox Channel interaction that the host app wants to report.
 * Multiple events can be sent together (bulk) in one /inbox/events request.
 *
 * @param eventType one of IMPRESSION (IM), OPEN (OP), CLICK (CL), DELETE (DT)
 * @param messageId the [InboxChannelMessage.id] (smsgId) the event belongs to
 * @param messageDetails the opaque token from [InboxChannelMessageData.messageDetails]
 */
data class InboxChannelEvent(
    val eventType: InboxChannelEventType,
    val messageId: String,
    val messageDetails: String?
) : Serializable
