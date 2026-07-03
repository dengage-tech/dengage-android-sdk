package com.dengage.sdk.domain.inboxchannel

import com.dengage.sdk.data.remote.api.ApiType
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEvent
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEventItem
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEventRequest
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelMessage
import com.dengage.sdk.util.Constants
import retrofit2.Response
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import java.util.TimeZone

class InboxChannelRepository {

    private val service: InboxChannelService by service(ApiType.PUSH)

    suspend fun getInboxChannelMessages(
        account: String,
        contactKey: String?,
        deviceId: String,
        appId: String?,
        limit: Int
    ): MutableList<InboxChannelMessage> {
        return service.getInboxChannelMessages(
            account = account,
            contactKey = contactKey,
            deviceId = deviceId,
            appId = appId,
            limit = limit
        )
    }

    suspend fun sendInboxChannelEvents(
        account: String,
        contactKey: String?,
        deviceId: String,
        appId: String?,
        events: List<InboxChannelEvent>
    ): Response<Unit> {
        val eventDateUTC = utcNow()
        val request = InboxChannelEventRequest(
            account = account,
            contactKey = contactKey,
            deviceId = deviceId,
            appId = appId,
            events = events.map { event ->
                InboxChannelEventItem(
                    eventType = event.eventType,
                    msgId = event.messageId,
                    messageDetails = event.messageDetails,
                    eventDateUTC = eventDateUTC
                )
            }
        )
        return service.sendInboxChannelEvents(request)
    }

    private fun utcNow(): String {
        return SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault()).apply {
            timeZone = TimeZone.getTimeZone("UTC")
        }.format(Date())
    }
}
