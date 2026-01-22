package com.dengage.sdk.manager.inappmessage

import java.util.*
import android.app.Activity
import android.content.Intent
import android.view.View
import androidx.core.net.toUri
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
import com.dengage.sdk.ui.inappmessage.Mustache
import com.dengage.sdk.ui.story.StoriesListView
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.extension.launchActivity
import com.dengage.sdk.domain.inappmessage.DebugLogRequest
import com.dengage.sdk.domain.inappmessage.DebugLoggingRepository
import com.dengage.sdk.manager.session.SessionManager
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import java.util.UUID

class InAppMessageManager :
    BaseMvpManager<InAppMessageContract.View, InAppMessageContract.Presenter>(),
    InAppMessageContract.View, InAppMessageActivity.InAppMessageCallback,
    InAppInlineElement.InAppMessageCallback, StoriesListView.InAppMessageCallback {

    override fun providePresenter() = InAppMessagePresenter()

    private var inAppMessageFetchCallback: InAppMessageFetchCallback? = null
    private val debugLoggingRepository = DebugLoggingRepository()
    private val debugScope = CoroutineScope(Dispatchers.IO)

    companion object {
        private var timer = Timer()
        private var hourlyFetchTimer: Timer? = null
        internal var isInAppMessageShowing = false
    }

    /**
     * Call this method for the pages that you should show in app message if available
     */
    internal fun setNavigation(
        activity: Activity,
        screenName: String? = null,
        params: HashMap<String, String>? = null,
        resultCode: Int = -1,
        inAppInlineElement: InAppInlineElement? = null,
        propertyId: String? = "",
        hideIfNotFound: Boolean? = false,
        storyPropertyId: String? = null,
        storiesListView: StoriesListView? = null,
    ) {
        // Check if an in-app message is already being displayed (skip inline messages)
        if (isInAppMessageShowing && inAppInlineElement == null && storiesListView == null) {
            DengageLogger.debug("setNavigation skipped: An in-app message is already being displayed")
            return
        }

        val sdkParameters = Prefs.sdkParameters
        if (sdkParameters != null) {
            val currentTime = System.currentTimeMillis()
            val fetchIntervalInMin = sdkParameters.inAppFetchIntervalInMin ?: 0
            val timeoutMinutes = maxOf(fetchIntervalInMin * 4, 60) // Use 1 hour minimum
            val timeoutMilliseconds = timeoutMinutes * 60 * 1000L

            val lastSuccessfulInAppFetch = Prefs.lastSuccessfulInAppMessageFetchTime
            val lastSuccessfulRealTimeFetch = Prefs.lastSuccessfulRealTimeInAppMessageFetchTime

            val timeSinceLastInAppFetch = currentTime - lastSuccessfulInAppFetch
            val timeSinceLastRealTimeFetch = currentTime - lastSuccessfulRealTimeFetch

            // If both fetches are older than the timeout, log warning and return
            if (timeSinceLastInAppFetch > timeoutMilliseconds && timeSinceLastRealTimeFetch > timeoutMilliseconds) {
                DengageLogger.warning("setNavigation blocked: No successful in-app message fetch in the last $timeoutMinutes minutes")
                return
            }
        }

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
                        showAppStory(priorInAppMessage, storiesListView)
                    }
                } else {
                    if(storiesListView == null) {
                        if (!"INLINE".equals(priorInAppMessage.data.content.type, ignoreCase = true) && inAppInlineElement != null) {
                           return
                        } else {

                            if (priorInAppMessage.data.content.params.html?.let {
                                Mustache.hasCouponSection(it)
                            } == true) {
                                val couponContent: String? = Mustache.getCouponContent(priorInAppMessage.data.content.params.html!!)

                                couponContent?.let { content ->
                                    // Mark as showing immediately to prevent duplicate calls during async validation
                                    isInAppMessageShowing = true
                                    presenter.validateCoupon(
                                        couponContent = content,
                                        inAppMessageId = priorInAppMessage.id,
                                        onValidCoupon = { couponCode ->
                                            showInAppMessage(
                                                activity,
                                                priorInAppMessage,
                                                resultCode,
                                                inAppInlineElement = inAppInlineElement,
                                                propertyId = propertyId,
                                                couponCode = couponCode
                                            )
                                        },
                                        onInvalidCoupon = { errorMessage ->
                                            isInAppMessageShowing = false
                                            DengageLogger.error("Coupon validation failed: $errorMessage")

                                            // Send debug log for invalid coupon if debug device
                                            sendCouponValidationFailureLog(
                                                couponContent = content,
                                                errorMessage = errorMessage,
                                                inAppMessage = priorInAppMessage,
                                                screenName = screenName
                                            )
                                        }
                                    )
                                }
                            } else {
                                showInAppMessage(
                                    activity,
                                    priorInAppMessage,
                                    resultCode,
                                    inAppInlineElement = inAppInlineElement,
                                    propertyId = propertyId,
                                    couponCode = null
                                )
                            }
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
        inAppInlineElement: InAppInlineElement?,
        couponCode: String? = null
    ) {
        try {
            // Mark as showing immediately to prevent duplicate calls
            if (inAppInlineElement == null) {
                isInAppMessageShowing = true
            }

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
                                if (couponCode != null) {
                                    InAppMessageActivity.newIntent(
                                        activity, inAppMessage, resultCode, couponCode
                                    )
                                } else {
                                    InAppMessageActivity.newIntent(
                                        activity, inAppMessage, resultCode
                                    )
                                }, resultCode
                            )

                            if (!inAppMessage.data.content.params.shouldAnimate) {
                                @Suppress("DEPRECATION")
                                activity.overridePendingTransition(0, 0)
                            }
                            InAppMessageActivity.inAppMessageCallback = this@InAppMessageManager
                        }

                    }
                }
            }, delay)
        } catch (e: Exception) {
            isInAppMessageShowing = false
            e.printStackTrace()
        } catch (e: Throwable) {
            isInAppMessageShowing = false
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

        if (isRealTime) {
            Prefs.lastSuccessfulRealTimeInAppMessageFetchTime = System.currentTimeMillis()
        } else {
            Prefs.lastSuccessfulInAppMessageFetchTime = System.currentTimeMillis()
        }

        inAppMessageFetchCallback?.inAppMessageFetched(isRealTime)

        if (inAppMessages != null) {

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

    internal fun startHourlyFetchTimer() {
        stopHourlyFetchTimer()

        val oneHourInMilliSeconds = 60 * 60 * 1000L
        hourlyFetchTimer = Timer().apply {
            schedule(object : TimerTask() {
                override fun run() {
                    if (DengageUtils.isAppInForeground()) {
                        fetchInAppMessages(null)
                    }
                    // Reschedule for next hour
                    startHourlyFetchTimer()
                }
            }, oneHourInMilliSeconds) // 1 hour in milliseconds
        }
    }

    internal fun stopHourlyFetchTimer() {
        hourlyFetchTimer?.cancel()
        hourlyFetchTimer?.purge()
        hourlyFetchTimer = null
    }

    override fun inAppMessageSetAsDisplayed() = Unit

    override fun inAppMessageSetAsClicked() = Unit

    override fun inAppMessageSetAsDismissed() = Unit

    override fun inAppMessageClicked(inAppMessage: InAppMessage, buttonId: String?, buttonType: String?) {
        isInAppMessageShowing = false
        setInAppMessageAsClicked(
            inAppMessage = inAppMessage, buttonId = buttonId, buttonType = buttonType
        )
    }

    override fun inAppMessageDismissed(inAppMessage: InAppMessage) {
        isInAppMessageShowing = false
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

    private fun showAppStory(inAppMessage: InAppMessage, storiesListView: StoriesListView) {
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
                        val intent = Intent(Intent.ACTION_VIEW, buttonUrl.toUri())
                        intent.putExtra("targetUrl", buttonUrl)
                        DengageUtils.sendBroadCast(intent.apply {
                            this.action = Constants.DEEPLINK_RETRIEVE_EVENT
                        }, ContextHolder.context)
                    } catch (e: Exception) {
                        DengageLogger.error(e.message)
                    }
                } else{
                    val intent = Intent(Intent.ACTION_VIEW, buttonUrl.toUri())
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

    private fun isDebugDevice(deviceId: String?, debugDeviceIds: List<String>?): Boolean {
        return !deviceId.isNullOrEmpty() && !debugDeviceIds.isNullOrEmpty() && debugDeviceIds.contains(deviceId)
    }

    private fun sendCouponValidationFailureLog(
        couponContent: String,
        errorMessage: String,
        inAppMessage: InAppMessage,
        screenName: String?
    ) {
        debugScope.launch {
            try {
                val subscription = Prefs.subscription
                val sdkParameters = Prefs.sdkParameters

                val isDebugDevice = isDebugDevice(
                    subscription?.getSafeDeviceId(),
                    sdkParameters?.debugDeviceIds
                )

                if (isDebugDevice) {
                    val traceId = UUID.randomUUID().toString()
                    val campaignId = inAppMessage.data.publicId ?: inAppMessage.id
                    
                    val debugLog = DebugLogRequest(
                        traceId = traceId,
                        appGuid = sdkParameters?.appId,
                        appId = sdkParameters?.appId,
                        account = sdkParameters?.accountName,
                        device = subscription?.getSafeDeviceId() ?: "",
                        sessionId = SessionManager.getSessionId(),
                        sdkVersion = DengageUtils.getSdkVersion(),
                        currentCampaignList = emptyList(),
                        campaignId = campaignId,
                        campaignType = if (inAppMessage.data.isRealTime()) "realtime" else "bulk",
                        sendId = null,
                        message = "Coupon validation failed: $couponContent - $errorMessage traceId:$traceId campaignId:$campaignId",
                        context = mapOf("coupon_code" to couponContent),
                        contactKey = subscription?.contactKey,
                        channel = "android",
                        currentRules = mapOf()
                    )

                    debugLoggingRepository.sendDebugLog(screenName ?: "unknown", debugLog)
                }
            } catch (e: Exception) {
                DengageLogger.error("Error sending coupon validation failure debug log: ${e.message}")
            }
        }
    }

}