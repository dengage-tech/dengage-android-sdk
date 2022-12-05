package com.dengage.sdk

import android.app.Activity
import android.content.Context
import android.content.Intent
import android.text.TextUtils
import com.dengage.sdk.callback.DengageCallback
import com.dengage.sdk.data.cache.Prefs
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
import com.dengage.sdk.manager.inappmessage.InAppMessageManager
import com.dengage.sdk.manager.inboxmessage.InboxMessageManager
import com.dengage.sdk.manager.rfm.RFMManager
import com.dengage.sdk.manager.subscription.SubscriptionManager
import com.dengage.sdk.manager.tag.TagManager
import com.dengage.sdk.ui.test.DengageTestActivity
import com.dengage.sdk.util.*
import com.dengage.sdk.util.extension.toJson
import com.google.firebase.FirebaseApp

object Dengage {

    val configurationManager by lazy { ConfigurationManager() }
    val subscriptionManager by lazy { SubscriptionManager() }
    private val inAppMessageManager by lazy { InAppMessageManager() }
    private val inboxMessageManager by lazy { InboxMessageManager() }
    private val tagManager by lazy { TagManager() }
    private val eventManager by lazy { EventManager() }
    private val rfmManager by lazy { RFMManager() }
    private val deviceIdSenderManager by lazy { DeviceIdSenderManager() }

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
        firebaseApp: FirebaseApp? = null
    ) {
        ContextHolder.context = context

        subscriptionManager.buildSubscription(
            firebaseIntegrationKey = firebaseIntegrationKey,
            huaweiIntegrationKey = huaweiIntegrationKey
        )
        val configurationCallback = object : ConfigurationCallback {
            override fun fetchInAppMessages() {
                inAppMessageManager.fetchInAppMessages()
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
            }
        }
        configurationManager.configurationCallback = configurationCallback

        configurationManager.init(
            firebaseApp = firebaseApp
        )
        configurationManager.getSdkParameters()
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
        subscriptionManager.setDeviceId(deviceId = deviceId)
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

        // clear inbox manager cache if contact key has been changed
        if (subscriptionManager.setContactKey(contactKey = contactKey)) {
            inboxMessageManager.clearInboxMessageCache()
        }
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
        dengageCallback: DengageCallback<MutableList<InboxMessage>>
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
        messageId: String
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
        messageId: String
    ) {
        inboxMessageManager.setInboxMessageAsClicked(
            messageId = messageId
        )
    }

    fun getInAppMessages() {
        inAppMessageManager.fetchInAppMessages()
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
        activity: Activity
    ) {
        setNavigation(
            activity = activity,
            screenName = null
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
        screenName: String? = null
    ) {
        inAppMessageManager.setNavigation(
            activity = activity,
            screenName = screenName
        )
    }

    /**
     * Send tags
     *
     * @param tags will be send to api
     */
    fun setTags(
        tags: List<TagItem>,context: Context? = null
    ) {
        ContextHolder.resetContext(context)
        tagManager.setTags(
            tags = tags
        )
    }

    fun onMessageReceived(data: Map<String, String?>?) {
        DengageLogger.verbose("onMessageReceived method is called")
        if (!data.isNullOrEmpty()) {
            val pushMessage = Message.createFromMap(data)
            val json = pushMessage.toJson()
            DengageLogger.verbose("Message Json: $json")
            val source = pushMessage.messageSource
            if (Constants.MESSAGE_SOURCE == source) {
                DengageLogger.debug("There is a message that received from dEngage")
                sendBroadcast(json, data)
            }
        }
    }

    fun sendBroadcast(json: String, data: Map<String, String?>) {
        DengageLogger.verbose("sendBroadcast method is called")
        try {
            val intent = Intent(Constants.PUSH_RECEIVE_EVENT)
            intent.putExtra("RAW_DATA", json)
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
    fun saveRFMScores(scores: MutableList<RFMScore>?,context: Context? = null) {
        ContextHolder.resetContext(context)
        rfmManager.saveRFMScores(
            scores = scores
        )
    }

    /**
     * Use for updating score of category
     */
    fun categoryView(categoryId: String,context: Context? = null) {
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

    fun pageView(data: HashMap<String, Any>,context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.pageView(
            eventDetails = data
        )
    }

    fun sendCartEvents(
        data: HashMap<String, Any>,
        eventType: String,context: Context? = null
    ) {
        ContextHolder.resetContext(context)
        eventManager.sendCartEvents(
            eventDetails = data,
            eventType = eventType
        )
    }

    fun addToCart(data: HashMap<String, Any>,context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.addToCart(
            eventDetails = data
        )
    }

    fun removeFromCart(data: HashMap<String, Any>,context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.removeFromCart(
            eventDetails = data
        )
    }

    fun viewCart(data: HashMap<String, Any>,context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.viewCart(
            eventDetails = data
        )
    }

    fun beginCheckout(data: HashMap<String, Any>,context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.beginCheckout(
            eventDetails = data
        )
    }

    fun cancelOrder(data: HashMap<String, Any>,context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.cancelOrder(
            eventDetails = data
        )
    }

    fun order(data: HashMap<String, Any>,context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.order(
            eventDetails = data
        )
    }

    fun search(data: HashMap<String, Any>,context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.search(
            eventDetails = data
        )
    }

    fun sendWishListEvents(
        data: HashMap<String, Any>,
        eventType: String,context: Context? = null
    ) {
        ContextHolder.resetContext(context)
        eventManager.sendWishListEvents(
            eventDetails = data,
            eventType = eventType
        )
    }

    fun addToWishList(data: HashMap<String, Any>,context: Context? = null) {
        ContextHolder.resetContext(context)
        eventManager.addToWishList(
            eventDetails = data
        )
    }

    fun removeFromWishList(data: HashMap<String, Any>,context: Context? = null) {
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
        data: HashMap<String, Any>,context: Context? = null
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
        data: HashMap<String, Any>
        ,context: Context? = null
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
        message: Message?
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
                    deviceIdSenderManager.sendDeviceId("$this$route", token)
                }
            }
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    fun setPartnerDeviceId(adid:String)
    {
        DengageLogger.verbose("setPartnerDeviceId method is called")
        subscriptionManager.setPartnerDeviceId(adid = adid)
    }
}