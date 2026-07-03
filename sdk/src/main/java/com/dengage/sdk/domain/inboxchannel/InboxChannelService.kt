package com.dengage.sdk.domain.inboxchannel

import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEventRequest
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelMessage
import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.GET
import retrofit2.http.Headers
import retrofit2.http.POST
import retrofit2.http.Query

interface InboxChannelService {

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @GET("/api/inbox/getMessages")
    suspend fun getInboxChannelMessages(
        @Query("acc") account: String,
        @Query("ckey") contactKey: String?,
        @Query("did") deviceId: String,
        @Query("appId") appId: String?,
        @Query("limit") limit: Int
    ): MutableList<InboxChannelMessage>

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @POST("/api/inbox/events")
    suspend fun sendInboxChannelEvents(
        @Body request: InboxChannelEventRequest
    ): Response<Unit>
}
