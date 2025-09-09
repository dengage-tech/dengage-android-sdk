package com.dengage.sdk

import android.Manifest
import android.annotation.SuppressLint
import android.app.Activity
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.os.Build
import android.os.Handler
import android.os.Looper
import android.text.TextUtils
import androidx.core.content.ContextCompat
import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.callback.ReviewDialogCallback
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.data.remote.api.ApiUrlConfiguration
import com.dengage.sdk.data.remote.api.DeviceConfigurationPreference
import com.dengage.sdk.data.remote.api.NotificationDisplayPriorityConfiguration
import com.dengage.sdk.domain.inappmessage.model.Cart
import com.dengage.sdk.domain.configuration.model.AppTracking
import com.dengage.sdk.domain.inboxmessage.model.InboxMessage
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.domain.rfm.model.RFMGender
import com.dengage.sdk.domain.rfm.model.RFMItem
import com.dengage.sdk.domain.rfm.model.RFMScore
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.manager.configuration.ConfigurationCallback
import com.dengage.sdk.manager.configuration.ConfigurationManager
import com.dengage.sdk.manager.deviceId.DeviceIdSenderManager
import com.dengage.sdk.manager.event.EventManager
import com.dengage.sdk.manager.inappmessage.InAppMessageFetchCallback
import com.dengage.sdk.manager.inappmessage.InAppMessageManager
import com.dengage.sdk.manager.inappmessage.session.InAppSessionManager
import com.dengage.sdk.manager.inappmessage.util.RealTimeInAppParamHolder
import com.dengage.sdk.manager.inboxmessage.InboxMessageManager
import com.dengage.sdk.manager.rfm.RFMManager
import com.dengage.sdk.manager.session.SessionManager
import com.dengage.sdk.manager.subscription.SubscriptionManager
import com.dengage.sdk.manager.tag.TagManager
import com.dengage.sdk.push.IDengageHmsManager
import com.dengage.sdk.push.NotificationPermissionActivity
import com.dengage.sdk.push.NotificationPermissionCallback
import com.dengage.sdk.push.clearNotification
import com.dengage.sdk.ui.inappmessage.InAppInlineElement
import com.dengage.sdk.ui.story.StoriesListView
import com.dengage.sdk.ui.test.DengageTestActivity
import com.dengage.sdk.util.*
import com.dengage.sdk.util.extension.shouldProcessPush
import com.dengage.sdk.util.extension.toJson
import com.google.android.play.core.review.ReviewInfo
import com.google.android.play.core.review.ReviewManager
import com.google.android.play.core.review.ReviewManagerFactory
import com.google.android.gms.tasks.Task
import com.google.firebase.FirebaseApp

@SuppressLint("StaticFieldLeak")
object Dengage {

    val configurationManager by lazy { ConfigurationManager() }
    val subscriptionManager by lazy { SubscriptionManager() }
    private val inAppMessageManager by lazy { InAppMessageManager() }
    private val inboxMessageManager by lazy { InboxMessageManager() }
    private val tagManager by lazy { TagManager() }
    private val eventManager by lazy { EventManager() }
    private val rfmManager by lazy { RFMManager() }
    private val deviceIdSenderManager by lazy { DeviceIdSenderManager() }
    private val inAppSessionManager by lazy { InAppSessionManager() }

    var initialized = false
        internal set
    private var isInAppFetched: Boolean = false
    private var currentActivity: Activity? = null

    /**
     * Use to init Fcm or Hms configuration and sdk parameters
     *
     * @param context for local cache operations
     * @param firebaseIntegrationKey for fcm operations, get firebase integration from dengage panel
     * @param huaweiIntegrationKey for hms operations,  get huawei integration from dengage panel
     * @param firebaseApp for fcm operations, it is optional parameter
     */
    fun init(
        context: Context,
        firebaseIntegrationKey: String? = null,
        huaweiIntegrationKey: String? = null,
        firebaseApp: FirebaseApp? = null,
        dengageHmsManager: IDengageHmsManager? = null,
        deviceId: String? = null,
        deviceConfigurationPreference: DeviceConfigurationPreference? = DeviceConfigurationPreference.Google,
        contactKey: String? = null,
        partnerDeviceId: String? = null,
        disableOpenWebUrl:Boolean? =false,
        notificationDisplayPriorityConfiguration: NotificationDisplayPriorityConfiguration =NotificationDisplayPriorityConfiguration.SHOW_WITH_DEFAULT_PRIORITY,
        apiUrlConfiguration: ApiUrlConfiguration? = null,
        initForGeofence: Boolean = false,
        ) {
        initialized = true
        ContextHolder.resetContext(context = context)
        SessionManager.getSessionId()

        subscriptionManager.buildSubscription(
            firebaseIntegrationKey,
            huaweiIntegrationKey,
            deviceId,
            deviceConfigurationPreference,
            contactKey,
            partnerDeviceId
        )

        val configurationCallback = object : ConfigurationCallback {
            override fun fetchInAppMessages() {
                inAppMessageManager.fetchInAppMessages(inAppMessageFetchCallbackParam = object :
                    InAppMessageFetchCallback {
                    override fun inAppMessageFetched(realTime: Boolean) {
                        isInAppFetched = true;
                    }

                })
            }

            override fun startAppTracking(appTrackings: List<AppTracking>?) {
                this@Dengage.startAppTracking(appTrackings)
                configurationManager.configurationCallback = null
            }

            override fun fetchInAppExpiredMessageIds() {
                inAppMessageManager.fetchInAppExpiredMessageIds()
            }

            override fun sendSubscription(subscription: Subscription) {
                subscriptionManager.saveSubscription(subscription)
                subscriptionManager.sendSubscription()
                inAppSessionManager.sendFirstLaunchEvent()
            }
        }
        configurationManager.configurationCallback = configurationCallback

        configurationManager.setDomain(apiUrlConfiguration, initForGeofence)

        Handler(Looper.getMainLooper()).postDelayed({
            configurationManager.init(
                dengageHmsManager = dengageHmsManager,
                firebaseApp = firebaseApp,
                firebaseIntegrationKey = firebaseIntegrationKey,
                huaweiIntegrationKey = huaweiIntegrationKey,
                deviceConfigurationPreference = deviceConfigurationPreference
            )
        }, 1000)

        configurationManager.saveOpenWebUrlConfigurations(disableOpenWebUrl)
        configurationManager.saveNotificationPriority(notificationDisplayPriorityConfiguration)
        configurationManager.getSdkParameters()
        DengageUtils.registerInAppBroadcast()
    }

    /**
     * Use to enable or disable logs on console.
     * @param enable true/false
     */
    fun setLogStatus(enable: Boolean) {
        Prefs.logVisibility = enable
    }

    fun setFirebaseIntegrationKey(integrationKey: String) {
        DengageLogger.verbose("setFirebaseIntegrationKey method is called")
        DengageLogger.verbose("setFirebaseIntegrationKey: $integrationKey")
        subscriptionManager.setFirebaseIntegrationKey(
            integrationKey = integrationKey
        )
    }

    fun setHuaweiIntegrationKey(integrationKey: String) {
        DengageLogger.verbose("setHuaweiIntegrationKey method is called")
        DengageLogger.verbose("setHuaweiIntegrationKey: $integrationKey")
        subscriptionManager.setHuaweiIntegrationKey(
            integrationKey = integrationKey
        )
    }

    /**
     * @return Subscription Object from cache.
     */
    fun getSubscription(): Subscription? {
        return Prefs.subscription
    }

    /**
     * Set Device Id
     * Use to set device id of current subscription
     * @param deviceId unique identifier of device
     */
    fun setDeviceId(deviceId: String) {
        DengageLogger.verbose("setDeviceId method is called")
        Handler(Looper.getMainLooper()).postDelayed({
            subscriptionManager.setDeviceId(deviceId = deviceId)
            inAppMessageManager.fetchVisitorInfo()
        }, 2000)

    }

    /**
     * Set Country
     * Use to set country of current subscription
     * @param country country string
     */
    fun setCountry(country: String) {
        DengageLogger.verbose("setCountry method is called")
        subscriptionManager.setCountry(country = country)
    }

    /**
     * Set contact key of the user subscription.
     * Use to set dEngage contactKey to a user subscription.
     * @param contactKey user key
     */
    fun setContactKey(contactKey: String?) {
        DengageLogger.verbose("setContactKey method is called")
        Handler(Looper.getMainLooper()).postDelayed({
        // clear inbox manager cache if contact key has been changed
        if (subscriptionManager.setContactKey(contactKey = contactKey)) {
            inboxMessageManager.clearInboxMessageCache()
        }
        }, 2000)
    }

    /**
     * Set User Push Permission
     * Use to set permission of current subscription
     * @param permission True/False
     */
    fun setUserPermission(permission: Boolean) {
        DengageLogger.verbose("setUserPermission method is called")
        subscriptionManager.setUserPermission(userPermission = permission)
    }

    /**
     * Get User Push Permission
     * Use to get permission of current subscription
     */
    fun getUserPermission(): Boolean? {
        return Prefs.subscription?.permission
    }

    /**
     * Set Token method
     * Use to set token of current subscription
     */
    fun setToken(token: String?) {
        DengageLogger.verbose("setToken method is called")
        subscriptionManager.setToken(token = token)
    }

    /**
     * Get Token method
     * Use to get token of current subscription
     */
    fun getToken(): String? {
        return Prefs.subscription?.token
    }

    fun onNewToken(token: String?) {
        DengageLogger.debug("On new token: $token")
        setToken(token = token)
    }

    /**
     * Set Notification Channel Name
     *
     * @param name will be saved in cache as channel name
     */
    fun setNotificationChannelName(name: String) {
        Prefs.notificationChannelName = name
    }

    fun startAppTracking(appTrackings: List<AppTracking>?) {
        configurationManager.getAppTrackingTags(
            appTrackings = appTrackings
        ).let { tagItems ->
            if (tagItems.isNotEmpty()) {
                setTags(tagItems)
            }
        }
    }

    /**
     * Get saved inbox messages
     */
    fun getInboxMessages(
        limit: Int,
        offset: Int,
        dengageCallback: DengageCallback<MutableList<InboxMessage>>,
    ) {
        inboxMessageManager.getInboxMessages(
            limit = limit,
            offset = offset,
            dengageCallback = dengageCallback
        )
    }

    /**
     * Delete inbox message
     *
     * @param messageId id of inbox message that will be deleted.
     */
    fun deleteInboxMessage(
        messageId: String,
    ) {
        inboxMessageManager.deleteInboxMessage(
            messageId = messageId
        )
    }

    /**
     * Mark inbox message as read
     *
     * @param messageId id of inbox message that will be marked as read.
     */
    fun setInboxMessageAsClicked(
        messageId: String,
    ) {
        inboxMessageManager.setInboxMessageAsClicked(
            messageId = messageId
        )
    }

    /**
     * Delete all inbox messages
     */
    fun deleteAllInboxMessages() {
        inboxMessageManager.deleteAllInboxMessages()
    }

    /**
     * Mark all inbox messages as read
     */
    fun setAllInboxMessagesAsClicked() {
        inboxMessageManager.setAllInboxMessagesAsClicked()
    }

    fun getInAppMessages() {
        inAppMessageManager.fetchInAppMessages(inAppMessageFetchCallbackParam = object :
            InAppMessageFetchCallback {
            override fun inAppMessageFetched(realTime: Boolean) {
                isInAppFetched = true
            }

        })
    }

    /**
     * Set cart for using in real time in app comparisons
     * 
     * @param cart Cart object containing cart items
     */
    fun setCart(cart: Cart) {
        RealTimeInAppParamHolder.setCart(cart)
    }
    
    /**
     * Get current cart
     * 
     * @return Cart object containing all cart items
     */
    fun getCart(): Cart {
        return RealTimeInAppParamHolder.getCart()
    }

    /**
     * Set category path for using in real time in app comparisons
     */
    fun setCategoryPath(path: String?) {
        RealTimeInAppParamHolder.categoryPath = path
    }

    /**
     * Set cart item count for using in real time in app comparisons
     */
    fun setCartItemCount(count: String?) {
        RealTimeInAppParamHolder.cartItemCount = count
    }

    /**
     * Set cart amount for using in real time in app comparisons
     */
    fun setCartAmount(amount: String?) {
        RealTimeInAppParamHolder.cartAmount = amount
    }

    /**
     * Set state for using in real time in app comparisons
     */
    fun setState(name: String?) {
        RealTimeInAppParamHolder.state = name
    }

    /**
     * Set city for using in real time in app comparisons
     */
    fun setCity(name: String?) {
        RealTimeInAppParamHolder.city = name
    }

    internal fun setLastSessionStartTime() {
        inAppSessionManager.setLastSessionStartTime()
    }

    internal fun setLastSessionDuration() {
        inAppSessionManager.setLastSessionDuration()
    }

    internal fun setLastVisitTime() {
        inAppSessionManager.setLastVisitTime()
    }

    internal fun sendAppForegroundEvent() {
        inAppSessionManager.sendAppForegroundEvent()
    }

    fun getInAppExpiredMessageIds() {
        inAppMessageManager.fetchInAppExpiredMessageIds()
    }

    /**
     * Show in app message if any available
     *
     * @param activity for showing ui of in app message
     */
    fun setNavigation(
        activity: Activity, resultCode: Int = -1,
    ) {
        setNavigation(
            activity = activity,
            screenName = null, resultCode
        )
    }

    /**
     * Show in app message if any available
     *
     * @param activity   for showing ui of in app message
     * @param screenName for showing screen specific in app message
     */
    fun setNavigation(
        activity: Activity,
        screenName: String? = null, resultCode: Int = -1,
    ) {
        inAppMessageManager.setNavigation(
            activity = activity,
            screenName = screenName, null, resultCode
        )
    }

    /**
     * Show in app message if any available
     *
     * @param activity   for showing ui of in app message
     * @param screenName for showing screen specific in app message
     * @param params for user specific in app message
     */
    fun showRealTimeInApp(
        activity: Activity,
        screenName: String? = null,
        params: HashMap<String, String>? = null, resultCode: Int = -1,
    ) {
        inAppMessageManager.setNavigation(
            activity = activity,
            screenName = screenName,
            params = params,
            resultCode

        )
    }

    /**
     * Send tags
     *
     * @param tags will be send to api
     */
    fun setTags(
        tags: List<TagItem>, context: Context? = null,
    ) {
        ContextHolder.resetContext(context)
        tagManager.setTags(
            tags = tags
        )
    }

    fun onMessageReceived(data: Map<String, String?>?) {
        if (data.isNullOrEmpty()) return

        try {
            DengageLogger.verbose("onMessageReceived method is called")

            val pushMessage = Message.createFromMap(data)

            if (pushMessage.shouldProcessPush()) {
                DengageUtils.registerBroadcast()
                val json = pushMessage.toJson()
                DengageLogger.verbose("Message Json: $json")

                if (pushMessage.messageSource == Constants.MESSAGE_SOURCE) {
                    DengageLogger.debug("There is a message that received from dEngage")
                    sendBroadcast(json, data)
                }
            }

        } catch (e: Exception) {
            DengageLogger.error("Exception in onMessageReceived: ${e.message}")
        } catch (t: Throwable) {
            DengageLogger.error("Throwable in onMessageReceived: ${t.message}")
        }
    }



    fun sendBroadcast(json: String, data: Map<String, String?>) {
        DengageLogger.verbose("sendBroadcast method is called")
        try {
            val intent = Intent(Constants.PUSH_RECEIVE_EVENT)
            intent.putExtra("RAW_DATA", json)
            intent.putExtra("requestCode", DengageUtils.generateRandomInt())
            DengageLogger.verbose("RAW_DATA: $json")
            for ((key, value) in data) {
                intent.putExtra(key, value)
            }
            intent.setPackage(ContextHolder.context.packageName)
            ContextHolder.context.sendBroadcast(intent)
        } catch (e: java.lang.Exception) {
            DengageLogger.error("sendBroadcast: " + e.message)
        }
    }

    /**
     * Use for testing some features of Dengage
     * Only for developers
     */
    fun showTestPage(activity: Activity) {
        activity.startActivity(Intent(activity, DengageTestActivity::class.java))
    }

    /**
     * Use for saving rfm scores to local storage if you will use rfm item sorting
     */
    fun saveRFMScores(scores: MutableList<RFMScore>?, context: Context? = null) {
        ContextHolder.resetContext(context)
        rfmManager.saveRFMScores(
            scores = scores
        )
    }

    /**
     * Use for updating score of category
     */
    fun categoryView(categoryId: String, context: Context? = null) {
        ContextHolder.resetContext(context)
        rfmManager.categoryView(
            categoryId = categoryId
        )
    }

    /**
     * Use for sorting rfm items with respect to rfm scores saved to local storage
     */
    fun <T> sortRFMItems(rfmGender: RFMGender, rfmItems: MutableList<RFMItem>): MutableList<T> {
        return rfmManager.sortRFMItems(
            rfmGender = rfmGender,
            rfmItems = rfmItems
        )
    }

    fun pageView(data: HashMap<String, Any>, context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.pageView(
            eventDetails = data
        )
    }

    fun sendCartEvents(
        data: HashMap<String, Any>,
        eventType: String, context: Context? = null,
    ) {
        ContextHolder.resetContext(context)
        eventManager.sendCartEvents(
            eventDetails = data,
            eventType = eventType
        )
    }

    fun addToCart(data: HashMap<String, Any>, context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.addToCart(
            eventDetails = data
        )
    }

    fun removeFromCart(data: HashMap<String, Any>, context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.removeFromCart(
            eventDetails = data
        )
    }

    fun viewCart(data: HashMap<String, Any>, context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.viewCart(
            eventDetails = data
        )
    }

    fun beginCheckout(data: HashMap<String, Any>, context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.beginCheckout(
            eventDetails = data
        )
    }

    fun cancelOrder(data: HashMap<String, Any>, context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.cancelOrder(
            eventDetails = data
        )
    }

    fun order(data: HashMap<String, Any>, context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.order(
            eventDetails = data
        )
    }

    fun search(data: HashMap<String, Any>, context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.search(
            eventDetails = data
        )
    }

    fun sendWishListEvents(
        data: HashMap<String, Any>,
        eventType: String, context: Context? = null,
    ) {
        ContextHolder.resetContext(context)
        eventManager.sendWishListEvents(
            eventDetails = data,
            eventType = eventType
        )
    }

    fun addToWishList(data: HashMap<String, Any>, context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.addToWishList(
            eventDetails = data
        )
    }

    fun removeFromWishList(data: HashMap<String, Any>, context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.removeFromWishList(
            eventDetails = data
        )
    }

    /**
     * Sends a custom event
     * <p>
     * Use to hit a custom event report.
     * </p>
     *
     * @param tableName The event table name of the schema.
     * @param key Value of the event key.
     * @param data Additional key-value data which is correspond table column name-value.
     */
    fun sendCustomEvent(
        tableName: String,
        key: String,
        data: HashMap<String, Any>, context: Context? = null,
    ) {
        ContextHolder.resetContext(context)
        eventManager.sendCustomEvent(
            tableName = tableName,
            key = key,
            eventDetails = data
        )
    }

    /**
     * Sends a device event
     * <p>
     * Use to hit a device event report.
     * </p>
     *
     * @param tableName The event table name of the schema.
     * @param data Additional key-value data which is correspond table column name-value.
     */
    fun sendDeviceEvent(
        tableName: String,
        data: HashMap<String, Any>, context: Context? = null,
    ) {
        ContextHolder.resetContext(context)
        eventManager.sendDeviceEvent(
            tableName = tableName,
            eventDetails = data
        )
    }

    /**
     * Sends open event
     *
     * Use to open report when a GCM message is received. Only required when you perform a manuel
     * GCM registration.
     *
     * @param message The dEngage message object.
     */
    fun sendOpenEvent(
        buttonId: String,
        itemId: String,
        message: Message?,
    ) {
        DengageLogger.verbose("sendOpenEvent method is called")
        DengageLogger.verbose(buttonId)
        DengageLogger.verbose(itemId)
        DengageLogger.verbose(message?.toJson())
        try {
            subscriptionManager.sendSubscription()
            getSubscription()
            if (message == null) {
                DengageLogger.error("Argument null: message")
                return
            }
            val source = message.messageSource
            if (Constants.MESSAGE_SOURCE != source) return

            if (!TextUtils.isEmpty(message.transactionId)) {

                eventManager.sendTransactionalOpenEvent(
                    buttonId = buttonId,
                    itemId = itemId,
                    messageId = message.messageId,
                    messageDetails = message.messageDetails,
                    transactionId = message.transactionId
                )
            } else {
                eventManager.sendOpenEvent(
                    buttonId = buttonId,
                    itemId = itemId,
                    messageId = message.messageId,
                    messageDetails = message.messageDetails
                )

                eventManager.sessionStart(message.targetUrl ?: "")
            }
        } catch (e: Exception) {
            DengageLogger.error("sendOpenEvent: " + e.message)
        }
    }

    /**
     * Send Login custom event
     */
    fun sendLoginEvent() {
        eventManager.sendLoginEvent()
    }

    /**
     * Send Logout custom event
     */
    fun sendLogoutEvent() {
        eventManager.sendLogoutEvent()
    }

    /**
     * Send Register custom event
     */
    fun sendRegisterEvent() {
        eventManager.sendRegisterEvent()
    }


    fun sendDeviceIdToServer(route: String, token: String) {
        try {
            DengageUtils.getMetaData(name = "den_device_id_api_url").apply {
                if (this == null) {
                    DengageLogger.error("Device id api base url not found on application manifest metadata")
                } else if (route.isNullOrEmpty()) {
                    DengageLogger.error("Device id api route is not provided")
                } else {
                    Constants.deviceToken = token
                    deviceIdSenderManager.sendDeviceId("$this$route", token)
                }
            }
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    fun setPartnerDeviceId(adid: String) {
        DengageLogger.verbose("setPartnerDeviceId method is called")
        Handler(Looper.getMainLooper()).postDelayed({
            subscriptionManager.setPartnerDeviceId(adid = adid)
        }, 2000)

    }

    fun inAppLinkConfiguration(
        inappDeeplink: String = "",
    ) {
        Prefs.inAppDeeplink = inappDeeplink
    }

    fun isInAppFetched(): Boolean {
        return isInAppFetched
    }

    fun getLastPushPayload(): String {
        val pushPayload = Prefs.lastPushPayload
        Prefs.lastPushPayload = null
        ContextHolder.context.applicationContext.clearNotification(pushPayload)
        return pushPayload?.toJson() ?: ""

    }

    fun setCurrentActivity(activity: Activity) {
        currentActivity = activity
    }

    internal fun getCurrentActivity(): Activity? {
        return currentActivity
    }

    fun restartApplicationAfterPushClick(restartApplication: Boolean) {
        Prefs.restartApplicationAfterPushClick = restartApplication
    }

    fun removeInAppMessageDisplay() {
        inAppMessageManager.cancelTimer()
    }

    fun setDevelopmentStatus(isDebug: Boolean? = false) {
        Prefs.isDevelopmentStatusDebug = isDebug
    }

    fun showRatingDialog(activity: Activity, reviewDialogCallback: ReviewDialogCallback) {
        val reviewManager: ReviewManager = ReviewManagerFactory.create(activity)
        val request: Task<ReviewInfo> = reviewManager.requestReviewFlow()
        request.addOnCompleteListener { task ->
            if (task.isSuccessful) {
                val reviewInfo: ReviewInfo = task.result
                val flow: Task<Void> = reviewManager.launchReviewFlow(activity, reviewInfo)
                flow.addOnCompleteListener {

                    reviewDialogCallback.onCompletion()
                }
            } else {
                reviewDialogCallback.onError()
            }
        }
    }

    fun requestNotificationPermission(activity: Activity)
    {
        if (Build.VERSION.SDK_INT >= 33) {
            if (ContextCompat.checkSelfPermission(activity, Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
                val callback = object : NotificationPermissionCallback {
                    override fun onPermissionResult(granted: Boolean) {
                        if (granted && Prefs.token.isNotEmpty()) {
                            setToken(Prefs.token)
                        } else {
                            setToken("")
                        }
                    }
                }
                NotificationPermissionActivity.callback = callback
                val intent = Intent(activity, NotificationPermissionActivity::class.java)
                activity.startActivity(intent)
            }
        }
    }

    fun setClassName(className:String)
    {
        Prefs.className=className
    }

    fun showInlineInApp(propertyId:String, inAppInlineElement: InAppInlineElement, activity: Activity, customParams: HashMap<String, String>? = null, screenName: String?,  hideIfNotFound:Boolean? = false)
    {
        inAppMessageManager.setNavigation(propertyId= propertyId,inAppInlineElement=inAppInlineElement, activity = activity, params = customParams, screenName = screenName, hideIfNotFound = hideIfNotFound)

    }

    fun showStoriesList(
        storyPropertyId: String,
        storiesListView: StoriesListView,
        activity: Activity,
        customParams: HashMap<String, String>? = null,
        screenName: String?
    ) {
        inAppMessageManager.setNavigation(
            storyPropertyId = storyPropertyId,
            storiesListView = storiesListView,
            activity = activity,
            params = customParams,
            screenName = screenName
        )

    }


    /**
     * Set language
     * Use to set language of current subscription
     * @param language language string
     */
    fun setLanguage(language: String) {
        DengageLogger.verbose("setLanguage method is called")
        subscriptionManager.setLanguage(language = language)
    }

    fun setInAppDeviceInfo(key: String, value: String) {
        val inAppDeviceInfoDic: MutableMap<String, String> =
            Prefs.inAppDeviceInfo ?: mutableMapOf()
        inAppDeviceInfoDic[key] = value
        Prefs.inAppDeviceInfo = inAppDeviceInfoDic
    }

    fun clearInAppDeviceInfo() {
        Prefs.inAppDeviceInfo = mutableMapOf()
    }

    fun getInAppDeviceInfo(): Map<String, String> {
       return Prefs.inAppDeviceInfo ?: mutableMapOf()
    }

    fun setLocationPermission(status: String) {
        DengageLogger.verbose("setLocationPermission method is called")
        subscriptionManager.setLocationPermission(status = status)
    }

    fun getSdkVersion(): String {
        return DengageUtils.getSdkVersion()
    }
}