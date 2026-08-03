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
import com.dengage.sdk.util.DengageAppStateTracker
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
        private var inSessionFetchTimer: Timer? = null
        internal var isInAppMessageShowing = false

        /** Ön plana dönüşler arasındaki minimum fetch aralığı. Geliştirme modunda uygulanmaz. */
        private const val APP_FOREGROUND_FETCH_FLOOR_MS = 10_000L

        /**
         * Oturum içi turlar arasındaki minimum bekleme. Gate henüz damgalanmamışken ya da
         * istek başarısız olup damga güncellenmemişken sıkı döngüye girmeyi engeller.
         */
        private const val MIN_IN_SESSION_FETCH_DELAY_MS = 60_000L

        /** Process içindeki son ön plan tetikli fetch zamanı; 0 ise henüz fetch yapılmadı. */
        @Volatile
        private var lastAppForegroundFetchTime = 0L
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
                hidePlacementIfNeeded(
                    inAppInlineElement, propertyId, storiesListView, storyPropertyId, hideIfNotFound
                )
                return
            }
        }

        if (propertyId.isNullOrEmpty()) {
            cancelTimer()
        }
        // control next in app message show time
        // Geliştirme modunda (manuel bayrak veya debug cihaz) gösterim aralığı uygulanmaz.
        if (!Prefs.isDevelopmentModeActive) {
            if (Prefs.inAppMessageShowTime != 0L && System.currentTimeMillis() < Prefs.inAppMessageShowTime) {
                hidePlacementIfNeeded(
                    inAppInlineElement, propertyId, storiesListView, storyPropertyId, hideIfNotFound
                )
                return
            }
        }

        val inAppMessages =
            InAppMessageUtils.findNotExpiredInAppMessages(Date(), Prefs.inAppMessages)
        Prefs.inAppMessages = inAppMessages
        if (inAppMessages.isNullOrEmpty()) {
            hidePlacementIfNeeded(
                inAppInlineElement, propertyId, storiesListView, storyPropertyId, hideIfNotFound
            )
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
            if (!storyPropertyId.isNullOrEmpty() && storiesListView != null) {
                val androidSelector = priorInAppMessage.data.inlineTarget?.androidSelector
                if (androidSelector == storyPropertyId && "STORY".equals(
                        priorInAppMessage.data.content.type,
                        ignoreCase = true
                    )
                ) {
                    showAppStory(
                        priorInAppMessage,
                        storiesListView,
                        storyPropertyId,
                        hideIfNotFound
                    )
                } else {
                    hidePlacementIfNeeded(
                        null, null, storiesListView, storyPropertyId, hideIfNotFound
                    )
                }
            } else {
                if (storiesListView == null) {
                    if (!"INLINE".equals(priorInAppMessage.data.content.type, ignoreCase = true) && inAppInlineElement != null) {
                        hidePlacementIfNeeded(
                            inAppInlineElement, propertyId, storiesListView, storyPropertyId, hideIfNotFound
                        )
                        return
                    } else {
                        if ("COUNTDOWN_TO_WIN".equals(priorInAppMessage.data.content.type, ignoreCase = true) &&
                            InAppMessageUtils.isCountdownToWinExpired(priorInAppMessage.data.content.params.html)
                        ) {
                            DengageLogger.debug("COUNTDOWN_TO_WIN in-app message is expired, skipping display")
                            hidePlacementIfNeeded(
                                inAppInlineElement, propertyId, storiesListView, storyPropertyId, hideIfNotFound
                            )
                            return
                        }
                        if (priorInAppMessage.data.content.params.html?.let {
                                Mustache.hasCouponSection(it)
                            } == true) {
                            val couponContent: String? =
                                Mustache.getCouponContent(priorInAppMessage.data.content.params.html!!)

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
                                        hidePlacementIfNeeded(
                                            inAppInlineElement, propertyId, storiesListView, storyPropertyId, hideIfNotFound
                                        )
                                    }
                                )
                            } ?: hidePlacementIfNeeded(
                                inAppInlineElement, propertyId, storiesListView, storyPropertyId, hideIfNotFound
                            )
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
        } else {
            hidePlacementIfNeeded(
                inAppInlineElement, propertyId, storiesListView, storyPropertyId, hideIfNotFound
            )
        }
    }

    private fun handleStoryNotFound(
        storiesListView: StoriesListView?,
        storyPropertyId: String?,
        hideIfNotFound: Boolean?,
    ) {
        if (storyPropertyId.isNullOrEmpty() || storiesListView == null) return
        storiesListView.clearContent()
        if (hideIfNotFound == true) {
            storiesListView.visibility = View.GONE
        }
    }

    private fun hidePlacementIfNeeded(
        inAppInlineElement: InAppInlineElement?,
        propertyId: String?,
        storiesListView: StoriesListView?,
        storyPropertyId: String?,
        hideIfNotFound: Boolean?,
    ) {
        handleStoryNotFound(storiesListView, storyPropertyId, hideIfNotFound)
        if (hideIfNotFound != true) return
        if (!propertyId.isNullOrEmpty()) {
            inAppInlineElement?.visibility = View.GONE
        }
    }

    /**
     * Fetch in app messages if enabled and fetch time is available
     */
    internal fun fetchInAppMessages(
        inAppMessageFetchCallbackParam: InAppMessageFetchCallback?,
        trigger: InAppFetchTrigger = InAppFetchTrigger.OTHER,
    ) {
        // Arka planda in-app çekilmez: kullanıcı ekranda olmadığı için mesaj gösterilemez ve
        // fetch interval'ı boşuna yanar.
        //
        // APP_FOREGROUND tetikleyicisi kapıdan muaftır: bu tetikleyici yalnızca lifecycle'ın
        // "ön plana geçiliyor" sinyalinden doğar, yani kendisi ön planda olmanın kanıtıdır.
        // Kapının Activity sayacı bu sinyalden bir adım geride kalabiliyor — host uygulama
        // DengageLifecycleTracker'ı bizim tracker'ımızdan önce kaydettiyse, Activity başladığında
        // önce host'un callback'i çalışıp fetch'i deniyor ve sayaç henüz 0 olduğu için ön plana
        // dönüş fetch'i sessizce düşüyordu.
        if (trigger != InAppFetchTrigger.APP_FOREGROUND && DengageAppStateTracker.shouldSkipRequest()) {
            DengageLogger.debug("fetchInAppMessages skipped, app is in background")
            return
        }
        // İlk kurulumda Activity, SDK parametreleri gelmeden önce açılır; presenter bu durumda
        // sessizce çıkar. Buradan dönmezsek o boş çağrı ön plan tabanını damgalar ve
        // parametreler geldiğinde tetiklenen **gerçek** fetch tabana takılır.
        if (Prefs.sdkParameters == null || Prefs.subscription == null) {
            DengageLogger.debug("fetchInAppMessages skipped, sdk parameters are not ready yet")
            return
        }
        // Ön plana geçiş her zaman fetch eder; yalnızca kazara arka plan/ön plan çalkantısını
        // eleyen küçük bir taban uygulanır.
        if (trigger == InAppFetchTrigger.APP_FOREGROUND && shouldSkipForAppForegroundFloor()) return

        // Geri çekilmenin sıfırlama çapası ön plana geçiştir.
        if (trigger == InAppFetchTrigger.APP_FOREGROUND) InAppFetchGate.reset()

        // Cleanup expired show history entries (older than 2 weeks)
        Prefs.cleanupExpiredShowHistory()
        val inappMessage = inAppMessageFetchCallbackParam
        inAppMessageFetchCallback = inappMessage
        presenter.getInAppMessages(
            bypassFetchInterval = trigger == InAppFetchTrigger.APP_FOREGROUND
        )
    }

    /**
     * Ön plan tabanı. Process içindeki **ilk** fetch koşulsuzdur — uygulamayı tamamen kapatıp
     * açmak her zaman fetch üretir, bu testçiye deterministik bir yol bırakır. Sonraki ön plana
     * dönüşler [APP_FOREGROUND_FETCH_FLOOR_MS] tabanına tabidir. Geliştirme modunda (manuel bayrak
     * ya da panel `debugDeviceIds`) taban sıfırdır: testçinin arka plan/ön plan döngüsü her
     * seferinde fetch üretir.
     */
    private fun shouldSkipForAppForegroundFloor(): Boolean {
        val now = System.currentTimeMillis()
        val floor = if (Prefs.isDevelopmentModeActive) 0L else APP_FOREGROUND_FETCH_FLOOR_MS
        val last = lastAppForegroundFetchTime
        if (last != 0L && now - last < floor) {
            val remainingSeconds = (floor - (now - last)) / 1000
            DengageLogger.debug(
                "fetchInAppMessages skipped by foreground floor, ${remainingSeconds}s remaining"
            )
            return true
        }
        lastAppForegroundFetchTime = now
        return false
    }

    internal fun fetchVisitorInfo() {
        presenter.getVisitorInfo()
    }


    /**
     * Fetch in app messages if enabled and fetch time is available
     */
    internal fun fetchCancelledInAppMessageIds() {
        if (DengageAppStateTracker.shouldSkipRequest()) {
            DengageLogger.debug("fetchCancelledInAppMessageIds skipped, app is in background")
            return
        }
        presenter.fetchCancelledInAppMessageIds()
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

    /**
     * Oturum içi periyodik tur. Sabit bir aralıkta tick atmak yerine timer doğrudan **gate'in
     * dolacağı ana** kurulur; her turdan sonra taze damgayla yeniden zamanlanır. Gecikme sık
     * tick + gate kontrolüyle aynı, ama saatte onlarca yerine birkaç ateşleme oluyor.
     */
    internal fun startInSessionFetchTimer() {
        stopInSessionFetchTimer()

        inSessionFetchTimer = Timer().apply {
            schedule(object : TimerTask() {
                override fun run() {
                    fetchInAppMessages(null)
                    startInSessionFetchTimer()
                }
            }, nextInSessionFetchDelay())
        }
    }

    internal fun stopInSessionFetchTimer() {
        inSessionFetchTimer?.cancel()
        inSessionFetchTimer?.purge()
        inSessionFetchTimer = null
    }

    /**
     * Bir sonraki turun ne kadar sonra atılacağı: bulk ve real-time gate'lerinden **önce dolanı**.
     * Damga henüz atılmamışsa (0) taban gecikmeye düşülür.
     */
    private fun nextInSessionFetchDelay(): Long {
        val now = System.currentTimeMillis()
        // Yalnızca damgalanmış kanallar sayılır. Kapalı bir kanalın damgası hiç yazılmaz ve 0
        // kalır; onu hesaba katmak timer'ı sonsuza dek taban gecikmede döndürürdü.
        val nextAllowed = listOf(Prefs.inAppMessageFetchTime, Prefs.realTimeInAppMessageFetchTime)
            .filter { it > 0L }
            .minOrNull()
            ?: return ((Prefs.sdkParameters?.inAppFetchIntervalInMin ?: 0) * 60_000L)
                .coerceAtLeast(MIN_IN_SESSION_FETCH_DELAY_MS)

        return (nextAllowed - now).coerceAtLeast(MIN_IN_SESSION_FETCH_DELAY_MS)
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

    private fun showAppStory(
        inAppMessage: InAppMessage,
        storiesListView: StoriesListView,
        storyPropertyId: String? = null,
        hideIfNotFound: Boolean? = false,
    ) {
        val data = inAppMessage.data
        if (!data.publicId.isNullOrEmpty() && !data.content.contentId.isNullOrEmpty()) {
            storiesListView.visibility = View.VISIBLE
            StoriesListView.inAppMessageCallback = this@InAppMessageManager
            storiesListView.loadInAppMessage(inAppMessage, data.publicId, data.content.contentId)
            presenter.sendStoryEvent(
                StoryEventType.DISPLAY,
                inAppMessage
            )
        } else {
            hidePlacementIfNeeded(null, null, storiesListView, storyPropertyId, hideIfNotFound)
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

                if (Prefs.isDebugDevice) {
                    val traceId = UUID.randomUUID().toString()
                    val campaignId = inAppMessage.data.publicId ?: inAppMessage.id

                    val debugLog = DebugLogRequest(
                        traceId = traceId,
                        appGuid = sdkParameters?.appId,
                        appId = sdkParameters?.appId,
                        account = sdkParameters?.accountName,
                        device = subscription?.getSafeDeviceId() ?: "",
                        // Debug log'u oturum döndürmemeli
                        sessionId = SessionManager.currentSessionId,
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