package com.dengage.sdk.manager.inappmessage

import java.util.*
import android.app.Activity
import android.content.Intent
import android.view.View
import androidx.core.net.toUri
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.AbTestAssignmentResponse
import com.dengage.sdk.domain.inappmessage.model.Content
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

        /**
         * Marker value stored in the sticky A/B assignment cache when the user was bucketed
         * into the control group. Kept distinct from any valid contentId GUID.
         */
        private const val CONTROL_GROUP_MARKER = "__CONTROL__"
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
                hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                return
            }
        }

        if (propertyId.isNullOrEmpty()) {
            cancelTimer()
        }
        // control next in app message show time
        if (Prefs.isDevelopmentStatusDebug == false) {
            if (Prefs.inAppMessageShowTime != 0L && System.currentTimeMillis() < Prefs.inAppMessageShowTime) {
                hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                return
            }
        }

        val inAppMessages =
            InAppMessageUtils.findNotExpiredInAppMessages(Date(), Prefs.inAppMessages)
        Prefs.inAppMessages = inAppMessages
        if (inAppMessages.isNullOrEmpty()) {
            hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
            return
        }
        val priorInAppMessage =
            InAppMessageUtils.findPriorInAppMessage(
                inAppMessages,
                screenName,
                params,
                propertyId,
                storyPropertyId
            )

        if (priorInAppMessage != null) {
            // A/B test campaigns need their variant materialized into `content` before the
            // normal type-based routing can run. Deterministic (single variant @ 100%) is
            // resolved locally; multi-variant campaigns require a fresh /ab/assign call.
            // Once resolved, the dispatcher (`dispatchPriorInAppMessage`) is invoked directly
            // — re-entering setNavigation would lose the in-memory `content` mutation, since
            // setNavigation re-reads Prefs.inAppMessages (fresh deserialization).
            if (priorInAppMessage.data.isAbTest() && priorInAppMessage.data.content == null) {
                resolveAbVariant(
                    activity = activity,
                    inAppMessage = priorInAppMessage,
                    resultCode = resultCode,
                    inAppInlineElement = inAppInlineElement,
                    propertyId = propertyId,
                    hideIfNotFound = hideIfNotFound,
                    screenName = screenName,
                    params = params,
                    storyPropertyId = storyPropertyId,
                    storiesListView = storiesListView
                )
                return
            }
            dispatchPriorInAppMessage(
                activity = activity,
                priorInAppMessage = priorInAppMessage,
                resultCode = resultCode,
                inAppInlineElement = inAppInlineElement,
                propertyId = propertyId,
                hideIfNotFound = hideIfNotFound,
                screenName = screenName,
                storyPropertyId = storyPropertyId,
                storiesListView = storiesListView
            )
        } else {
            hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
        }
    }

    /**
     * Routes a resolved campaign through the appropriate display path: story, inline, or
     * full-screen with coupon/countdown handling. Extracted so that A/B-test campaigns can
     * skip [setNavigation]'s up-front fetch/filter pipeline (which would clobber the
     * in-memory variant we just materialized) and dispatch straight from the resolution
     * callback.
     */
    private fun dispatchPriorInAppMessage(
        activity: Activity,
        priorInAppMessage: InAppMessage,
        resultCode: Int,
        inAppInlineElement: InAppInlineElement?,
        propertyId: String?,
        hideIfNotFound: Boolean?,
        screenName: String?,
        storyPropertyId: String?,
        storiesListView: StoriesListView?,
    ) {
        if (!storyPropertyId.isNullOrEmpty() && storiesListView != null) {
                val androidSelector = priorInAppMessage.data.inlineTarget?.androidSelector
                if (androidSelector == storyPropertyId && "STORY".equals(
                        priorInAppMessage.data.content?.type,
                        ignoreCase = true
                    )
                ) {
                    showAppStory(priorInAppMessage, storiesListView)
                }
            } else {
                if (storiesListView == null) {
                    if (!"INLINE".equals(priorInAppMessage.data.content?.type, ignoreCase = true) && inAppInlineElement != null) {
                        hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                        return
                    } else {
                        if ("COUNTDOWN_TO_WIN".equals(priorInAppMessage.data.content?.type, ignoreCase = true) &&
                            InAppMessageUtils.isCountdownToWinExpired(priorInAppMessage.data.content?.params?.html)
                        ) {
                            DengageLogger.debug("COUNTDOWN_TO_WIN in-app message is expired, skipping display")
                            hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                            return
                        }
                        if (priorInAppMessage.data.content?.params?.html?.let {
                                Mustache.hasCouponSection(it)
                            } == true) {
                            val couponContent: String? =
                                Mustache.getCouponContent(priorInAppMessage.data.content!!.params.html!!)

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
                                            couponCode = couponCode,
                                            hideIfNotFound = hideIfNotFound
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
                                        hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                                    }
                                )
                            } ?: hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                        } else {
                            showInAppMessage(
                                activity,
                                priorInAppMessage,
                                resultCode,
                                inAppInlineElement = inAppInlineElement,
                                propertyId = propertyId,
                                couponCode = null,
                                hideIfNotFound = hideIfNotFound
                            )
                        }
                    }
                }
            }
    }

    /**
     * Resolve the A/B variant for an A/B campaign and dispatch the materialized content.
     *
     * - Single variant at 100% (winner phase or single-bucket config) is deterministic:
     *   no /ab/assign call is needed, the lone variant is rendered.
     * - Control-group buckets render nothing.
     * - Active phase (>1 variants) requires /ab/assign to pick the variant.
     */
    private fun resolveAbVariant(
        activity: Activity,
        inAppMessage: InAppMessage,
        resultCode: Int,
        inAppInlineElement: InAppInlineElement?,
        propertyId: String?,
        hideIfNotFound: Boolean?,
        screenName: String?,
        params: HashMap<String, String>?,
        storyPropertyId: String?,
        storiesListView: StoriesListView?,
    ) {
        val abTest = inAppMessage.data.abTest
        val variants = abTest?.variants
        if (abTest == null || variants.isNullOrEmpty()) {
            hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
            return
        }

        // Deterministic path: single variant at 100% (winner phase or single-bucket config).
        if (abTest.isDeterministic()) {
            val only = variants[0]
            if (only.isControlGroup) {
                // Control bucket — render nothing.
                hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                return
            }
            val materialized = Content.fromVariant(only)
            if (materialized == null) {
                DengageLogger.warning("A/B deterministic variant has no renderable content")
                hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                return
            }
            inAppMessage.data.content = materialized
            dispatchPriorInAppMessage(
                activity = activity,
                priorInAppMessage = inAppMessage,
                resultCode = resultCode,
                inAppInlineElement = inAppInlineElement,
                propertyId = propertyId,
                hideIfNotFound = hideIfNotFound,
                screenName = screenName,
                storyPropertyId = storyPropertyId,
                storiesListView = storiesListView
            )
            return
        }

        // Active phase: ask the backend which variant to render for this impression.
        val campaignId = inAppMessage.data.publicId
        if (campaignId.isNullOrEmpty()) {
            hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
            return
        }

        // Sticky cache: backend doesn't guarantee user-level stickiness (counts are aggregate,
        // deficit-weighted random can return different variants for back-to-back calls). To
        // keep the same user on the same variant for the lifetime of an active A/B test, we
        // persist the first assignment and reuse it on every subsequent impression. Once the
        // campaign reaches the winner phase, isDeterministic() short-circuits above and the
        // cache is bypassed entirely.
        cachedAbAssignment(campaignId)?.let { cachedContentId ->
            if (cachedContentId == CONTROL_GROUP_MARKER) {
                hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                return
            }
            applyAssignedVariant(
                inAppMessage = inAppMessage,
                variants = variants,
                assignedContentId = cachedContentId,
                activity = activity,
                resultCode = resultCode,
                inAppInlineElement = inAppInlineElement,
                propertyId = propertyId,
                hideIfNotFound = hideIfNotFound,
                screenName = screenName,
                storyPropertyId = storyPropertyId,
                storiesListView = storiesListView
            )
            return
        }

        presenter.assignAbVariant(
            campaignId = campaignId,
            onAssigned = { response ->
                if (response.isControlBucket()) {
                    rememberAbAssignment(campaignId, CONTROL_GROUP_MARKER)
                    hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                    return@assignAbVariant
                }
                val assignedId = response.contentId
                if (assignedId.isNullOrEmpty()) {
                    hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
                    return@assignAbVariant
                }
                rememberAbAssignment(campaignId, assignedId)
                applyAssignedVariant(
                    inAppMessage = inAppMessage,
                    variants = variants,
                    assignedContentId = assignedId,
                    activity = activity,
                    resultCode = resultCode,
                    inAppInlineElement = inAppInlineElement,
                    propertyId = propertyId,
                    hideIfNotFound = hideIfNotFound,
                    screenName = screenName,
                    storyPropertyId = storyPropertyId,
                    storiesListView = storiesListView
                )
            },
            onError = {
                // /ab/assign failed (network / 404). Skip this impression — backend already
                // skipped event logging for it. Don't poison the sticky cache on failure.
                hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
            }
        )
    }

    private fun applyAssignedVariant(
        inAppMessage: InAppMessage,
        variants: List<com.dengage.sdk.domain.inappmessage.model.AbTestVariant>,
        assignedContentId: String,
        activity: Activity,
        resultCode: Int,
        inAppInlineElement: InAppInlineElement?,
        propertyId: String?,
        hideIfNotFound: Boolean?,
        screenName: String?,
        storyPropertyId: String?,
        storiesListView: StoriesListView?,
    ) {
        val variant = variants.firstOrNull {
            !it.isControlGroup && it.contentId != null && it.contentId.equals(assignedContentId, ignoreCase = true)
        }
        if (variant == null) {
            // Defensive: payload and assignment can briefly disagree across cache refreshes.
            DengageLogger.warning("A/B assignment contentId not found among local variants; skipping impression")
            hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
            return
        }
        val materialized = Content.fromVariant(variant)
        if (materialized == null) {
            hideInlineIfNeeded(inAppInlineElement, propertyId, hideIfNotFound)
            return
        }
        inAppMessage.data.content = materialized
        dispatchPriorInAppMessage(
            activity = activity,
            priorInAppMessage = inAppMessage,
            resultCode = resultCode,
            inAppInlineElement = inAppInlineElement,
            propertyId = propertyId,
            hideIfNotFound = hideIfNotFound,
            screenName = screenName,
            storyPropertyId = storyPropertyId,
            storiesListView = storiesListView
        )
    }

    /**
     * Returns the sticky variant assignment persisted for this campaign, or null if there
     * is no cache entry. Entries survive across sessions and app restarts; they are only
     * superseded when the campaign reaches the deterministic winner phase (which skips the
     * cache entirely).
     */
    @Synchronized
    private fun cachedAbAssignment(campaignId: String): String? {
        return Prefs.abTestAssignments[campaignId]
    }

    @Synchronized
    private fun rememberAbAssignment(campaignId: String, contentId: String) {
        val assignments = Prefs.abTestAssignments
        assignments[campaignId] = contentId
        Prefs.abTestAssignments = assignments
    }

    private fun hideInlineIfNeeded(
        inAppInlineElement: InAppInlineElement?,
        propertyId: String?,
        hideIfNotFound: Boolean?,
    ) {
        if (hideIfNotFound != true) return
        if (propertyId.isNullOrEmpty()) return
        inAppInlineElement?.visibility = View.GONE
    }

    /**
     * Fetch in app messages if enabled and fetch time is available
     */
    internal fun fetchInAppMessages(inAppMessageFetchCallbackParam: InAppMessageFetchCallback?) {
        // Cleanup expired show history entries (older than 2 weeks)
        Prefs.cleanupExpiredShowHistory()
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
        couponCode: String? = null,
        hideIfNotFound: Boolean? = false,
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
                            Prefs.updateInAppMessageShowCount(inAppMessage.id, inAppMessage.data.showCount)
                        } else {
                            if (inAppMessage.data.isRealTime()) {
                                inAppMessage.data.showCount += 1
                                updateInAppMessageOnCache(inAppMessage)
                                Prefs.updateInAppMessageShowCount(inAppMessage.id, inAppMessage.data.showCount)
                            } else {
                                inAppMessage.data.showCount += 1
                                Prefs.updateInAppMessageShowCount(inAppMessage.id, inAppMessage.data.showCount)
                                removeInAppMessageFromCache(inAppMessageId = inAppMessage.id)
                            }
                        }

                        // update next in app message show time
                        Prefs.inAppMessageShowTime =
                            System.currentTimeMillis() + ((Prefs.sdkParameters?.inAppMinSecBetweenMessages
                                ?: 0) * 1000)
                        if (inAppMessage.data.inlineTarget?.androidSelector == propertyId) {
                            inAppInlineElement?.visibility = View.VISIBLE
                            inAppInlineElement?.populateInLineInApp(inAppMessage, activity)
                            InAppInlineElement.inAppMessageCallback = this@InAppMessageManager

                        } else {
                            if (inAppInlineElement != null && hideIfNotFound == true && !propertyId.isNullOrEmpty()) {
                                inAppInlineElement.visibility = View.GONE
                            }
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

                            if (inAppMessage.data.content?.params?.shouldAnimate != true) {
                                @Suppress("DEPRECATION")
                                activity.overridePendingTransition(0, 0)
                            }
                            InAppMessageActivity.inAppMessageCallback = this@InAppMessageManager
                        }

                        // For A/B campaigns the `content` we set on this object was just a
                        // per-impression resolution of one variant; the canonical payload has
                        // `content == null` and carries variants under `abTest`. Strip the
                        // materialized content out before persisting so the next impression
                        // re-enters /ab/assign instead of replaying the previous variant.
                        if (inAppMessage.data.isAbTest() && inAppMessage.data.content != null) {
                            inAppMessage.data.content = null
                            if (inAppMessage.data.isRealTime()) {
                                updateInAppMessageOnCache(inAppMessage)
                            }
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
                    val showHistory = Prefs.inAppMessageShowHistory

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

                    // find new ones and add them, checking history for showCount
                    val newInAppMessages = inAppMessages.filter { inAppMessage ->
                        existingInAppMessages.firstOrNull { existingInAppMessage ->
                            existingInAppMessage.id == inAppMessage.id
                        } == null
                    }
                    // Restore showCount from history for new messages
                    newInAppMessages.forEach { newMsg ->
                        showHistory[newMsg.id]?.let { historyEntry ->
                            newMsg.data.showCount = historyEntry.showCount
                        }
                    }
                    existingInAppMessages.addAll(newInAppMessages)
                } else {
                    val showHistory = Prefs.inAppMessageShowHistory
                    val updatedMessages = inAppMessages.map { newMsg ->
                        val oldMsg = existingInAppMessages.firstOrNull { it.id == newMsg.id }
                        if (oldMsg != null) {
                            newMsg.data.nextDisplayTime = oldMsg.data.nextDisplayTime
                            newMsg.data.showCount = oldMsg.data.showCount
                            newMsg.data.dismissCount = oldMsg.data.dismissCount
                        } else {
                            // Check show history for messages not in cache
                            showHistory[newMsg.id]?.let { historyEntry ->
                                newMsg.data.showCount = historyEntry.showCount
                            }
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
        val storyContentId = data.content?.contentId
        if(!data.publicId.isNullOrEmpty() && !storyContentId.isNullOrEmpty()) {
            StoriesListView.inAppMessageCallback = this@InAppMessageManager
            storiesListView.loadInAppMessage(inAppMessage, data.publicId, storyContentId)
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
        if (!data.publicId.isNullOrEmpty() && !data.content?.contentId.isNullOrEmpty()) {
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

    override fun getViewedStoryIds(storyCoverId: String): List<String> {
        return Prefs.shownStoryDic?.get(storyCoverId)?.toList() ?: emptyList()
    }

    override fun setLastViewedStoryIndex(storyCoverId: String, index: Int) {
        val dic = Prefs.lastViewedStoryIndexDic ?: mutableMapOf()
        dic[storyCoverId] = index
        Prefs.lastViewedStoryIndexDic = dic
    }

    override fun getLastViewedStoryIndex(storyCoverId: String): Int {
        return Prefs.lastViewedStoryIndexDic?.get(storyCoverId) ?: -1
    }

    override fun setStoryViewed(
        storyId: String,
        storyCoverId: String,
        storySetId: String,
        allStoryIdsInCover: List<String>
    ) {
        val shownStoryDic: MutableMap<String, MutableList<String>> =
            Prefs.shownStoryDic ?: mutableMapOf()
        val seenStoryIds = shownStoryDic.getOrPut(storyCoverId) { mutableListOf() }
        if (storyId !in seenStoryIds) {
            seenStoryIds.add(storyId)
        }
        Prefs.shownStoryDic = shownStoryDic

        if (allStoryIdsInCover.isNotEmpty() && seenStoryIds.containsAll(allStoryIdsInCover)) {
            setStoryCoverShown(storyCoverId, storySetId)
        }
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