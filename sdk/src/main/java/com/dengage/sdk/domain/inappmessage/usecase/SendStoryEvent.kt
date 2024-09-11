package com.dengage.sdk.domain.inappmessage.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inappmessage.RealTimeInAppMessageRepository
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.util.createLazy
import retrofit2.Response

enum class StoryEventType(val type: String) {
    DISPLAY("ds"),
    STORY_DISPLAY("sd"),
    STORY_CLICK("sc")
}

class SendStoryEvent : CoroutineUseCase<Response<Unit>, SendStoryEvent.Params>() {

    private val repository: RealTimeInAppMessageRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.sendStoryEvent(
            accountName = params!!.accountName,
            subscription = params.subscription,
            appId = params.appId,
            sessionId = params.sessionId,
            campaignId = params.campaignId,
            messageDetails = params.messageDetails,
            contentId = params.contentId,
            storyProfileId = params.storyProfileId,
            storyProfileName = params.storyProfileName,
            storyId = params.storyId,
            storyName = params.storyName,
            storyEventType = params.storyEventType.type
        )

    data class Params(
        val accountName: String?,
        val subscription: Subscription,
        val appId: String?,
        val sessionId: String,
        val campaignId: String,
        val messageDetails: String?,
        val contentId: String?,
        val storyProfileId: String?,
        val storyProfileName: String?,
        val storyId: String?,
        val storyName: String?,
        val storyEventType: StoryEventType
    )
}
