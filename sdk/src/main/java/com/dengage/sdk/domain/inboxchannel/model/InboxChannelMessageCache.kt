package com.dengage.sdk.domain.inboxchannel.model

import java.io.Serializable

/**
 * Lightweight local record of an Inbox Channel message's interaction state,
 * persisted in [com.dengage.sdk.data.cache.Prefs] so read/deleted state
 * survives across fetches.
 */
data class InboxChannelMessageCache(
    val id: String,
    var isRead: Boolean,
    var isDeleted: Boolean,
    val receiveDate: String?
) : Serializable
