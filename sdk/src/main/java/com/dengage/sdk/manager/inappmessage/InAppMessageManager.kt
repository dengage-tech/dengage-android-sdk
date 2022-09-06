package com.dengage.sdk.manager.inappmessage

import android.app.Activity
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.manager.base.BaseMvpManager
import com.dengage.sdk.manager.inappmessage.util.InAppMessageUtils
import com.dengage.sdk.ui.inappmessage.InAppMessageActivity
import java.util.*

class InAppMessageManager : BaseMvpManager<InAppMessageContract.View, InAppMessageContract.Presenter>(),
    InAppMessageContract.View, InAppMessageActivity.InAppMessageCallback {

    override fun providePresenter() = InAppMessagePresenter()

    /**
     * Call this method for the pages that you should show in app message if available
     */
    internal fun setNavigation(
        activity: Activity,
        screenName: String? = null,
        params: HashMap<String, String>? = null
    ) {
        // control next in app message show time
        if (Prefs.inAppMessageShowTime != 0L && System.currentTimeMillis() < Prefs.inAppMessageShowTime) return

        val inAppMessages =
            InAppMessageUtils.findNotExpiredInAppMessages(Date(), Prefs.inAppMessages)
        Prefs.inAppMessages = inAppMessages
        if (!inAppMessages.isNullOrEmpty()) {
            val priorInAppMessage =
                InAppMessageUtils.findPriorInAppMessage(inAppMessages, screenName, params)
            if (priorInAppMessage != null) {
                showInAppMessage(activity, priorInAppMessage)
            }
        }
    }

    /**
     * Fetch in app messages if enabled and fetch time is available
     */
    internal fun fetchInAppMessages() {
        presenter.getInAppMessages()
    }

    /**
     * Call service for setting in app message as displayed
     */
    private fun setInAppMessageAsDisplayed(inAppMessage: InAppMessage) {
        presenter.setInAppMessageAsDisplayed(
            inAppMessage = inAppMessage
        )
    }

    /**
     * Call service for setting in app message as clicked
     */
    private fun setInAppMessageAsClicked(
        inAppMessage: InAppMessage,
        buttonId: String?
    ) {
        presenter.setInAppMessageAsClicked(
            inAppMessage = inAppMessage,
            buttonId = buttonId
        )
    }

    /**
     * Call service for setting in app message as dismissed
     */
    private fun setInAppMessageAsDismissed(inAppMessage: InAppMessage) {
        presenter.setInAppMessageAsDismissed(
            inAppMessage = inAppMessage
        )
    }

    /**
     * Show in app message dialog on activity screen
     */
    private fun showInAppMessage(activity: Activity, inAppMessage: InAppMessage) {
        setInAppMessageAsDisplayed(
            inAppMessage = inAppMessage
        )

        if (inAppMessage.data.displayTiming.showEveryXMinutes != null &&
            inAppMessage.data.displayTiming.showEveryXMinutes != 0
        ) {
            inAppMessage.data.nextDisplayTime = System.currentTimeMillis() +
                inAppMessage.data.displayTiming.showEveryXMinutes * 60000L
            updateInAppMessageOnCache(inAppMessage)
        } else {
            removeInAppMessageFromCache(inAppMessageId = inAppMessage.id)
        }

        // update next in app message show time
        Prefs.inAppMessageShowTime = System.currentTimeMillis() +
            ((Prefs.sdkParameters?.inAppMinSecBetweenMessages ?: 0) * 1000)

        // set delay for showing in app message
        val delay = (inAppMessage.data.displayTiming.delay ?: 0) * 1000L
        Timer().schedule(object : TimerTask() {
            override fun run() {
                activity.runOnUiThread {
                    activity.startActivity(InAppMessageActivity.newIntent(activity, inAppMessage))
                    if (!inAppMessage.data.content.params.shouldAnimate) {
                        activity.overridePendingTransition(0, 0)
                    }
                    InAppMessageActivity.inAppMessageCallback = this@InAppMessageManager
                }
            }
        }, delay)
    }

    private fun updateInAppMessageOnCache(inAppMessage: InAppMessage) {
        val inAppMessages = Prefs.inAppMessages
        inAppMessages?.removeAll { message -> message.id == inAppMessage.id }
        inAppMessages?.add(inAppMessage)
        Prefs.inAppMessages = inAppMessages
    }

    private fun removeInAppMessageFromCache(inAppMessageId: String) {
        val inAppMessages = Prefs.inAppMessages
        inAppMessages?.removeAll { inAppMessage -> inAppMessage.id == inAppMessageId }
        Prefs.inAppMessages = inAppMessages
    }

    override fun fetchedInAppMessages(inAppMessages: MutableList<InAppMessage>?) {
        if (!inAppMessages.isNullOrEmpty()) {
            // get existing in app messages and save with fetched in app messages
            var existingInAppMessages = Prefs.inAppMessages
            if (existingInAppMessages == null) {
                existingInAppMessages = mutableListOf()
            } else {
                // remove duplicated in app messages
                existingInAppMessages.removeAll { existingInAppMessage ->
                    inAppMessages.firstOrNull { inAppMessage ->
                        inAppMessage.id == existingInAppMessage.id
                    } != null
                }
            }
            existingInAppMessages.addAll(inAppMessages)

            Prefs.inAppMessages = existingInAppMessages
        }
    }

    override fun inAppMessageSetAsDisplayed() = Unit

    override fun inAppMessageSetAsClicked() = Unit

    override fun inAppMessageSetAsDismissed() = Unit

    override fun inAppMessageClicked(inAppMessage: InAppMessage, buttonId: String?) {
        setInAppMessageAsClicked(
            inAppMessage = inAppMessage,
            buttonId = buttonId
        )
    }

    override fun inAppMessageDismissed(inAppMessage: InAppMessage) {
        setInAppMessageAsDismissed(
            inAppMessage = inAppMessage
        )
    }

    override fun sendTags(tags: String?) {
        // todo send tags
    }

}
