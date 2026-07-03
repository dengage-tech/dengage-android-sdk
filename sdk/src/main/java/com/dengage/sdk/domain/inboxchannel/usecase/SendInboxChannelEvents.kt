package com.dengage.sdk.domain.inboxchannel.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inboxchannel.InboxChannelRepository
import com.dengage.sdk.domain.inboxchannel.model.InboxChannelEvent
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SendInboxChannelEvents :
    CoroutineUseCase<Response<Unit>, SendInboxChannelEvents.Params>() {

    private val repository: InboxChannelRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.sendInboxChannelEvents(
            account = params!!.account,
            contactKey = params.contactKey,
            deviceId = params.deviceId,
            appId = params.appId,
            events = params.events
        )

    data class Params(
        val account: String,
        val contactKey: String?,
        val deviceId: String,
        val appId: String?,
        val events: List<InboxChannelEvent>
    )
}
