package com.dengage.sdk.manager.inappmessage

import android.app.Activity
import android.content.Intent
import android.net.Uri
import android.view.View
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.model.StoryCover
import com.dengage.sdk.domain.inappmessage.usecase.StoryEventType
import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.manager.base.BaseMvpManager
import com.dengage.sdk.manager.inappmessage.util.InAppMessageUtils
import com.dengage.sdk.ui.inappmessage.InAppInlineElement
import com.dengage.sdk.ui.inappmessage.InAppMessageActivity
import com.dengage.sdk.ui.story.StoriesListView
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.extension.launchActivity
import java.util.*

class InAppMessageManager :
    BaseMvpManager<InAppMessageContract.View, InAppMessageContract.Presenter>(),
    InAppMessageContract.View, InAppMessageActivity.InAppMessageCallback,
    InAppInlineElement.InAppMessageCallback, StoriesListView.InAppMessageCallback {

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
        resultCode: Int = -1,
        isRealTime: Boolean = false,
        inAppInlineElement: InAppInlineElement? = null,
        propertyId: String? = "",
        hideIfNotFound: Boolean? = false,
        storyPropertyId: String? = null,
        storiesListView: StoriesListView? = null,
    ) {
        if (propertyId.isNullOrEmpty()) {
            cancelTimer()
        }
        // control next in app message show time
        if (Prefs.isDevelopmentStatusDebug == false) {
            if (Prefs.inAppMessageShowTime != 0L && System.currentTimeMillis() < Prefs.inAppMessageShowTime) return
        }
        val inAppMessages =
            InAppMessageUtils.findNotExpiredInAppMessages(Date(), Prefs.inAppMessages)
        Prefs.inAppMessages = inAppMessages
        if (!inAppMessages.isNullOrEmpty()) {
            val priorInAppMessage =
                InAppMessageUtils.findPriorInAppMessage(
                    inAppMessages,
                    screenName,
                    params,
                    isRealTime,
                    propertyId,
                    storyPropertyId
                )

            if (priorInAppMessage != null) {
                if (!storyPropertyId.isNullOrEmpty() && storiesListView != null) {
                    val androidSelector = priorInAppMessage.data.inlineTarget?.androidSelector
                    if (androidSelector == storyPropertyId && "STORY".equals(
                            priorInAppMessage.data.content.type,
                            ignoreCase = true
                        )
                    ) {
                        showAppStory(activity, priorInAppMessage, storiesListView)
                    }
                } else {
                    if(storiesListView == null) {
                        if (!"INLINE".equals(priorInAppMessage.data.content.type, ignoreCase = true) && inAppInlineElement != null) {
                           return
                        } else {
                            showInAppMessage(
                                activity,
                                priorInAppMessage,
                                resultCode,
                                inAppInlineElement = inAppInlineElement,
                                propertyId = propertyId
                            )
                        }
                    }
                }

            } else if (!propertyId.isNullOrEmpty() && hideIfNotFound == true) {
                inAppInlineElement?.visibility = View.GONE
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

    internal fun fetchVisitorInfo() {
        presenter.getVisitorInfo()
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
        inAppMessage: InAppMessage, buttonId: String?, buttonType: String?
    ) {
        presenter.setInAppMessageAsClicked(
            inAppMessage = inAppMessage, buttonId = buttonId, buttonType = buttonType
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
        activity: Activity,
        inAppMessage: InAppMessage,
        resultCode: Int = -1,
        propertyId: String? = "",
        inAppInlineElement: InAppInlineElement?
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
                            inAppMessage.data.showCount += 1
                            updateInAppMessageOnCache(inAppMessage)
                        } else {
                            if (inAppMessage.data.isRealTime()) {
                                inAppMessage.data.showCount += 1
                                updateInAppMessageOnCache(inAppMessage)
                            } else {
                                removeInAppMessageFromCache(inAppMessageId = inAppMessage.id)
                            }
                        }

                        // update next in app message show time
                        Prefs.inAppMessageShowTime =
                            System.currentTimeMillis() + ((Prefs.sdkParameters?.inAppMinSecBetweenMessages
                                ?: 0) * 1000)
                        if (inAppMessage.data.inlineTarget?.androidSelector == propertyId) {

                            inAppInlineElement?.populateInLineInApp(inAppMessage, activity)
                            InAppInlineElement.inAppMessageCallback = this@InAppMessageManager

                        } else {
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
                            val dismissCount = it.data.dismissCount
                            it.data = inAppMessage.data
                            it.data.nextDisplayTime = nextDisplayTime
                            it.data.showCount = showCount
                            it.data.dismissCount = dismissCount
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
                    val updatedMessages = inAppMessages.map { newMsg ->
                        val oldMsg = existingInAppMessages.firstOrNull { it.id == newMsg.id }
                        if (oldMsg != null) {
                            newMsg.data.nextDisplayTime = oldMsg.data.nextDisplayTime
                            newMsg.data.showCount = oldMsg.data.showCount
                            newMsg.data.dismissCount = oldMsg.data.dismissCount
                        }
                        newMsg
                    }

                    existingInAppMessages.removeAll { existingMsg ->
                        inAppMessages.any { newMsg -> newMsg.id == existingMsg.id }
                    }

                    existingInAppMessages.addAll(updatedMessages)
                }
            }

            Prefs.inAppMessages = existingInAppMessages
        }
    }

    override fun inAppMessageSetAsDisplayed() = Unit

    override fun inAppMessageSetAsClicked() = Unit

    override fun inAppMessageSetAsDismissed() = Unit

    override fun inAppMessageClicked(inAppMessage: InAppMessage, buttonId: String?, buttonType: String?) {
        setInAppMessageAsClicked(
            inAppMessage = inAppMessage, buttonId = buttonId, buttonType = buttonType
        )
    }

    override fun inAppMessageDismissed(inAppMessage: InAppMessage) {
        setInAppMessageAsDismissed(
            inAppMessage = inAppMessage
        )
    }

    override fun sendTags(tags: List<TagItem>?) {
        if (!tags.isNullOrEmpty()) {
            Dengage.setTags(tags)
        }
    }

    fun cancelTimer() {
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

    private fun showAppStory(activity: Activity, inAppMessage: InAppMessage, storiesListView: StoriesListView) {
        val data = inAppMessage.data
        if(!data.publicId.isNullOrEmpty() && !data.content.contentId.isNullOrEmpty()) {
            StoriesListView.inAppMessageCallback = this@InAppMessageManager
            storiesListView.loadInAppMessage(inAppMessage, data.publicId, data.content.contentId)
            presenter.sendStoryEvent(
                StoryEventType.DISPLAY,
                inAppMessage
            )
        }
    }

    override fun storyEvent(
        eventType: StoryEventType,
        inAppMessage: InAppMessage,
        storyProfileId: String,
        storyProfileName: String,
        storyId: String,
        storyName: String,
        buttonUrl: String
    ) {
        val data = inAppMessage.data
        if (!data.publicId.isNullOrEmpty() && !data.content.contentId.isNullOrEmpty()) {
            presenter.sendStoryEvent(
                eventType,
                inAppMessage,
                storyProfileId,
                storyProfileName,
                storyId,
                storyName
            )

            if(eventType == StoryEventType.STORY_CLICK) {
                if (DengageUtils.isDeeplink(buttonUrl)) {
                    try {
                        val intent = Intent(Intent.ACTION_VIEW, Uri.parse(buttonUrl))
                        intent.putExtra("targetUrl", buttonUrl)
                        DengageUtils.sendBroadCast(intent.apply {
                            this.action = Constants.DEEPLINK_RETRIEVE_EVENT
                        }, ContextHolder.context)
                    } catch (e: Exception) {
                        DengageLogger.error(e.message)
                    }
                } else{
                    val intent = Intent(Intent.ACTION_VIEW, Uri.parse(buttonUrl))
                    intent.putExtra("targetUrl", buttonUrl)
                    ContextHolder.context.launchActivity(intent, buttonUrl)
                }
            }
        }

    }

    override fun setStoryCoverShown(storyCoverId: String, storySetId: String) {
        val shownStoryCoverDic: MutableMap<String, MutableList<String>> =
            Prefs.shownStoryCoverDic ?: mutableMapOf()

        if (!shownStoryCoverDic.containsKey(storySetId)) {
            shownStoryCoverDic[storySetId] = ArrayList()
        }
        if (!shownStoryCoverDic[storySetId]!!.contains(storyCoverId)) {
            shownStoryCoverDic[storySetId]!!.add(storyCoverId)
        }
        Prefs.shownStoryCoverDic = shownStoryCoverDic
    }

    override fun sortStoryCovers(storyCovers: List<StoryCover>, storySetId: String): List<StoryCover>{
        val shownStoryCoverDic: MutableMap<String, MutableList<String>> =
            Prefs.shownStoryCoverDic ?: mutableMapOf()

        if (shownStoryCoverDic.containsKey(storySetId)) {
            val shownStoryCoverIds = shownStoryCoverDic[storySetId]
            val notShownStoryCovers: MutableList<StoryCover> = ArrayList<StoryCover>()
            val shownStoryCovers: MutableList<StoryCover> = ArrayList<StoryCover>()
            if (!shownStoryCoverIds.isNullOrEmpty()) {
                for (cover in storyCovers) {
                    if (shownStoryCoverIds.contains(cover.id)) {
                        cover.shown = true
                        shownStoryCovers.add(cover)
                    } else {
                        notShownStoryCovers.add(cover)
                    }
                }
                notShownStoryCovers.addAll(shownStoryCovers)
                return notShownStoryCovers
            }
        }
        return storyCovers
    }

}