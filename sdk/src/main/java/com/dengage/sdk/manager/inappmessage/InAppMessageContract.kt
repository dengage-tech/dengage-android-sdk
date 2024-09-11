package com.dengage.sdk.manager.inappmessage

import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.model.StoryCover
import com.dengage.sdk.domain.inappmessage.usecase.StoryEventType
import com.dengage.sdk.manager.base.BasePresenter
import com.dengage.sdk.manager.base.BaseView

interface InAppMessageContract {

    interface View : BaseView {
        fun fetchedInAppMessages(inAppMessages: MutableList<InAppMessage>?, isRealTime: Boolean)
        fun inAppMessageSetAsDisplayed()
        fun inAppMessageSetAsClicked()
        fun inAppMessageSetAsDismissed()
    }

    interface Presenter : BasePresenter<View> {
        fun getInAppMessages()
        fun setInAppMessageAsDisplayed(inAppMessage: InAppMessage)
        fun setInAppMessageAsClicked(
            inAppMessage: InAppMessage,
            buttonId: String?
        )

        fun sendStoryEvent(
            storyEventType: StoryEventType,
            inAppMessage: InAppMessage,
            storyProfileId: String? = null,
            storyProfileName: String? = null,
            storyId: String? = null,
            storyName: String? = null
        )

        fun setStoryCoverShown(storyCoverId: String, storySetId: String)
        fun sortStoryCovers(storyCovers: List<StoryCover>, storySetId: String): List<StoryCover>

        fun getVisitorInfo()


        fun fetchInAppExpiredMessageIds()
        fun setInAppMessageAsDismissed(inAppMessage: InAppMessage)
    }

}