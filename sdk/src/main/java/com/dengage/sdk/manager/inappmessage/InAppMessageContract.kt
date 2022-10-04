package com.dengage.sdk.manager.inappmessage

import com.dengage.sdk.domain.inappmessage.model.InAppMessage
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

        fun setInAppMessageAsDismissed(inAppMessage: InAppMessage)
    }

}