package com.dengage.sdk.manager.inappmessage

import android.app.Activity
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.manager.base.BaseMvpManager
import com.dengage.sdk.manager.inappmessage.util.InAppMessageUtils
import com.dengage.sdk.ui.inappmessage.InAppMessageActivity
import java.util.*

class InAppMessageManager :
    BaseMvpManager<InAppMessageContract.View, InAppMessageContract.Presenter>(),
    InAppMessageContract.View, InAppMessageActivity.InAppMessageCallback {

    override fun providePresenter() = InAppMessagePresenter()

    private var inAppMessageFetchCallback: InAppMessageFetchCallback? = null

    companion object {
        private var timer = Timer()
    }

    /**
     * Call this method for the pages that you should show in app message if available
     */
    internal fun setNavigation(
        activity: Activity,
        screenName: String? = null,
        params: HashMap<String, String>? = null,
        resultCode: Int = -1
    ) {
        cancelTimer()

        // control next in app message show time
        if (Prefs.inAppMessageShowTime != 0L && System.currentTimeMillis() < Prefs.inAppMessageShowTime) return

        val inAppMessages =
            InAppMessageUtils.findNotExpiredInAppMessages(Date(), Prefs.inAppMessages)
        Prefs.inAppMessages = inAppMessages
        if (!inAppMessages.isNullOrEmpty()) {
            val priorInAppMessage =
                InAppMessageUtils.findPriorInAppMessage(inAppMessages, screenName, params)
            if (priorInAppMessage != null) {
                showInAppMessage(activity, priorInAppMessage, resultCode)
            }
        }
    }

    /**
     * Fetch in app messages if enabled and fetch time is available
     */
    internal fun fetchInAppMessages(inAppMessageFetchCallbackParam: InAppMessageFetchCallback?) {
        val inappMessage = inAppMessageFetchCallbackParam
        inAppMessageFetchCallback = inappMessage
        presenter.getInAppMessages()
    }


    /**
     * Fetch in app messages if enabled and fetch time is available
     */
    internal fun fetchInAppExpiredMessageIds() {
        presenter.fetchInAppExpiredMessageIds()
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
        inAppMessage: InAppMessage, buttonId: String?
    ) {
        presenter.setInAppMessageAsClicked(
            inAppMessage = inAppMessage, buttonId = buttonId
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
    private fun showInAppMessage(
        activity: Activity, inAppMessage: InAppMessage, resultCode: Int = -1
    ) {
        try {
            // set delay for showing in app message
            val delay = (inAppMessage.data.displayTiming.delay ?: 0) * 1000L
            timer.schedule(object : TimerTask() {
                override fun run() {
                    activity.runOnUiThread {
                        setInAppMessageAsDisplayed(
                            inAppMessage = inAppMessage
                        )

                        if (inAppMessage.data.displayTiming.showEveryXMinutes != null && inAppMessage.data.displayTiming.showEveryXMinutes != 0) {
                            inAppMessage.data.nextDisplayTime =
                                System.currentTimeMillis() + inAppMessage.data.displayTiming.showEveryXMinutes!! * 60000L
                            inAppMessage.data.showCount = inAppMessage.data.showCount + 1
                            updateInAppMessageOnCache(inAppMessage)
                        } else {
                            if (inAppMessage.data.isRealTime()) {
                                inAppMessage.data.showCount = inAppMessage.data.showCount + 1
                                updateInAppMessageOnCache(inAppMessage)
                            } else {
                                removeInAppMessageFromCache(inAppMessageId = inAppMessage.id)
                            }
                        }

                        // update next in app message show time
                        Prefs.inAppMessageShowTime =
                            System.currentTimeMillis() + ((Prefs.sdkParameters?.inAppMinSecBetweenMessages
                                ?: 0) * 1000)

                        activity.startActivityForResult(
                            InAppMessageActivity.newIntent(
                                activity, inAppMessage, resultCode
                            ), resultCode
                        )


                        if (!inAppMessage.data.content.params.shouldAnimate) {
                            activity.overridePendingTransition(0, 0)
                        }
                        InAppMessageActivity.inAppMessageCallback = this@InAppMessageManager
                    }
                }
            }, delay)
        } catch (e: Exception) {
            e.printStackTrace()
        } catch (e: Throwable) {
            e.printStackTrace()
        }

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

    override fun fetchedInAppMessages(
        inAppMessages: MutableList<InAppMessage>?, isRealTime: Boolean
    ) {
        inAppMessageFetchCallback?.inAppMessageFetched(isRealTime)

        if (!inAppMessages.isNullOrEmpty()) {
            var existingInAppMessages = Prefs.inAppMessages
            if (existingInAppMessages == null) {
                existingInAppMessages = mutableListOf()
                existingInAppMessages.addAll(inAppMessages)
            } else {
                if (isRealTime) {
                    // remove non existing real time in app messages
                    existingInAppMessages.removeAll { existingInAppMessage ->
                        existingInAppMessage.data.isRealTime() && inAppMessages.firstOrNull { inAppMessage ->
                            inAppMessage.id == existingInAppMessage.id
                        } == null
                    }

                    // find duplicated ones and update them
                    val updateInAppMessages = inAppMessages.filter { inAppMessage ->
                        existingInAppMessages.firstOrNull { existingInAppMessage ->
                            existingInAppMessage.id == inAppMessage.id
                        } != null
                    }

                    // update duplicated ones on existing in app messages list and don't update some parameters
                    updateInAppMessages.forEach { inAppMessage ->
                        val existingInAppMessage = existingInAppMessages.firstOrNull {
                            it.id == inAppMessage.id
                        }
                        existingInAppMessage?.let {
                            val nextDisplayTime = it.data.nextDisplayTime
                            val showCount = it.data.showCount
                            it.data = inAppMessage.data
                            it.data.nextDisplayTime = nextDisplayTime
                            it.data.showCount = showCount
                        }
                    }

                    // find new ones and add them
                    val newInAppMessages = inAppMessages.filter { inAppMessage ->
                        existingInAppMessages.firstOrNull { existingInAppMessage ->
                            existingInAppMessage.id == inAppMessage.id
                        } == null
                    }
                    existingInAppMessages.addAll(newInAppMessages)
                } else {
                    // remove duplicated in app messages
                    existingInAppMessages.removeAll { existingInAppMessage ->
                        inAppMessages.firstOrNull { inAppMessage ->
                            inAppMessage.id == existingInAppMessage.id
                        } != null
                    }
                    existingInAppMessages.addAll(inAppMessages)
                }
            }

            Prefs.inAppMessages = existingInAppMessages
        }
    }

    override fun inAppMessageSetAsDisplayed() = Unit

    override fun inAppMessageSetAsClicked() = Unit

    override fun inAppMessageSetAsDismissed() = Unit

    override fun inAppMessageClicked(inAppMessage: InAppMessage, buttonId: String?) {
        setInAppMessageAsClicked(
            inAppMessage = inAppMessage, buttonId = buttonId
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

    private fun cancelTimer() {
        try {
            timer.cancel()
            timer.purge()
            timer = Timer()

        } catch (e: Exception) {
            e.printStackTrace()
        } catch (e: Throwable) {
            e.printStackTrace()
        }
    }

}