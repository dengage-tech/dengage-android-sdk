package com.dengage.sdk.manager.event

import android.net.Uri
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.manager.base.BaseMvpManager
import com.dengage.sdk.manager.inappmessage.util.RealTimeInAppParamHolder
import com.dengage.sdk.manager.session.SessionManager
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import java.util.*

class EventManager : BaseMvpManager<EventContract.View, EventContract.Presenter>(),
    EventContract.View {

    override fun providePresenter() = EventPresenter()

    private var isSessionStarted = false

    internal fun sessionStart(referer: String) {
        if (isSessionStarted) return
        try {
            val data = HashMap<String, Any>()
            try {
                val uri = Uri.parse(referer)
                uri.getQueryParameter("utm_source")?.let {
                    data["utm_source"] = it
                }
                uri.getQueryParameter("utm_medium")?.let {
                    data["utm_medium"] = it
                }
                uri.getQueryParameter("utm_campaign")?.let {
                    data["utm_campaign"] = it
                }
                uri.getQueryParameter("utm_content")?.let {
                    data["utm_content"] = it
                }
                uri.getQueryParameter("utm_term")?.let {
                    data["utm_term"] = it
                }
                uri.getQueryParameter("gclid")?.let {
                    data["gclid"] = it
                }
                uri.getQueryParameter("dn_channel")?.let {
                    data["channel"] = it
                }
                uri.getQueryParameter("dn_send_id")?.let {
                    data["send_id"] = it
                }
                uri.getQueryParameter("dn_camp_id")?.let {
                    data["camp_id"] = it
                }
            } catch (e: Exception) {
                DengageLogger.error(e.message)
            }
            sendDeviceEvent(EventTable.SESSION_INFO.tableName, data)
            isSessionStarted = true
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    internal fun pageView(eventDetails: HashMap<String, Any>) {
        try {
            if (!eventDetails.containsKey(EventKey.PAGE_TYPE.key)) {
                DengageLogger.error("data must have a valid page_type parameter")
                return
            }
            sendDeviceEvent(EventTable.PAGE_VIEW_EVENTS.tableName, eventDetails)

            RealTimeInAppParamHolder.addPageView()
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    internal fun sendCartEvents(
        eventDetails: HashMap<String, Any>,
        eventType: String
    ) {
        try {
            val copyData: HashMap<String, Any> = HashMap(eventDetails)
            copyData.remove(EventKey.CART_ITEMS.key)

            val eventId = UUID.randomUUID().toString()
            if (!copyData.containsKey(EventKey.EVENT_TYPE.key)) {
                copyData.remove(EventKey.EVENT_TYPE.key)
            }
            if (!copyData.containsKey(EventKey.EVENT_ID.key)) {
                copyData.remove(EventKey.EVENT_ID.key)
            }
            copyData[EventKey.EVENT_TYPE.key] = eventType
            copyData[EventKey.EVENT_ID.key] = eventId
            sendDeviceEvent(EventTable.SHOPPING_CART_EVENTS.tableName, copyData)

            if (eventDetails.containsKey(EventKey.CART_ITEMS.key)) {
                val items = eventDetails[EventKey.CART_ITEMS.key] as Array<Any>?
                for (obj in items!!) {
                    if (obj is HashMap<*, *>) {
                        val product = obj as HashMap<String, Any>
                        product[EventKey.EVENT_ID.key] = eventId
                        sendDeviceEvent(EventTable.SHOPPING_CART_EVENTS_DETAIL.tableName, product)
                    }
                }
            }
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    internal fun addToCart(eventDetails: HashMap<String, Any>) {
        sendCartEvents(
            eventDetails = eventDetails,
            eventType = EventType.ADD_TO_CART.type
        )
    }

    internal fun removeFromCart(eventDetails: HashMap<String, Any>) {
        sendCartEvents(
            eventDetails = eventDetails,
            eventType = EventType.REMOVE_FROM_CART.type
        )
    }

    internal fun viewCart(eventDetails: HashMap<String, Any>) {
        sendCartEvents(
            eventDetails = eventDetails,
            eventType = EventType.VIEW_CART.type
        )
    }

    internal fun beginCheckout(eventDetails: HashMap<String, Any>) {
        sendCartEvents(
            eventDetails = eventDetails,
            eventType = EventType.BEGIN_CHECKOUT.type
        )
    }

    internal fun cancelOrder(eventDetails: HashMap<String, Any>) {
        try {
            val copyData: HashMap<String, Any> = HashMap(eventDetails)
            copyData.remove(EventKey.CART_ITEMS.key)

            if (copyData.containsKey(EventKey.EVENT_TYPE.key)) {
                copyData.remove(EventKey.EVENT_TYPE.key)
            }
            copyData[EventKey.EVENT_TYPE.key] = EventType.CANCEL.type
            if (copyData.containsKey(EventKey.TOTAL_AMOUNT.key)) {
                if(copyData[EventKey.TOTAL_AMOUNT.key].toString().toIntOrNull()!=null) {
                    val totalAmount: Int = copyData[EventKey.TOTAL_AMOUNT.key] as Int
                    copyData[EventKey.TOTAL_AMOUNT.key] = totalAmount.unaryMinus()
                }
            }
            sendDeviceEvent(EventTable.ORDER_EVENTS.tableName, copyData)

            if (eventDetails.containsKey(EventKey.CART_ITEMS.key)) {
                val items = eventDetails[EventKey.CART_ITEMS.key] as Array<Any>?
                for (obj in items!!) {
                    if (obj is HashMap<*, *>) {
                        val product = obj as HashMap<String, Any>
                        if (copyData.containsKey(EventKey.ORDER_ID.key)) {
                            product[EventKey.ORDER_ID.key] =
                                copyData[EventKey.ORDER_ID.key].toString()
                        }
                        sendDeviceEvent(EventTable.ORDER_EVENTS_DETAIL.tableName, product)
                    }
                }
            }
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    internal fun order(eventDetails: HashMap<String, Any>) {
        try {
            val copyData: HashMap<String, Any> = HashMap(eventDetails)
            copyData.remove(EventKey.CART_ITEMS.key)

            if (!copyData.containsKey(EventKey.EVENT_TYPE.key)) {
                copyData.remove(EventKey.EVENT_TYPE.key)
            }
            copyData[EventKey.EVENT_TYPE.key] = EventType.ORDER.type
            sendDeviceEvent(EventTable.ORDER_EVENTS.tableName, copyData)

            val eventId = DengageUtils.generateUUID()
            val cartEventParams = HashMap<String, Any>()
            cartEventParams[EventKey.EVENT_TYPE.key] = EventType.ORDER.type
            cartEventParams[EventKey.EVENT_ID.key] = eventId
            sendDeviceEvent(EventTable.SHOPPING_CART_EVENTS.tableName, cartEventParams)

            if (eventDetails.containsKey(EventKey.CART_ITEMS.key)) {
                val items = eventDetails[EventKey.CART_ITEMS.key] as Array<Any>?
                for (obj in items!!) {
                    if (obj is HashMap<*, *>) {
                        val product = obj as HashMap<String, Any>
                        if (copyData.containsKey(EventKey.ORDER_ID.key)) {
                            product[EventKey.ORDER_ID.key] =
                                copyData[EventKey.ORDER_ID.key].toString()
                        }
                        sendDeviceEvent(EventTable.ORDER_EVENTS_DETAIL.tableName, product)
                    }
                }
            }
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    internal fun search(eventDetails: HashMap<String, Any>) {
        sendDeviceEvent(
            tableName = EventTable.SEARCH_EVENTS.tableName,
            eventDetails = eventDetails
        )
    }

    internal fun sendWishListEvents(
        eventDetails: HashMap<String, Any>,
        eventType: String
    ) {
        try {
            val copyData: HashMap<String, Any> = HashMap(eventDetails)
            copyData.remove(EventKey.ITEMS.key)

            val eventId = DengageUtils.generateUUID()

            if (!copyData.containsKey(EventKey.EVENT_TYPE.key)) {
                copyData.remove(EventKey.EVENT_TYPE.key)
            }
            if (!copyData.containsKey(EventKey.EVENT_ID.key)) {
                copyData.remove(EventKey.EVENT_ID.key)
            }

            copyData[EventKey.EVENT_TYPE.key] = eventType
            copyData[EventKey.EVENT_ID.key] = eventId
            sendDeviceEvent(EventTable.WISHLIST_EVENTS.tableName, copyData)
            if (eventDetails.containsKey(EventKey.ITEMS.key)) {
                val items = eventDetails[EventKey.ITEMS.key] as Array<Any>?
                for (obj in items!!) {
                    if (obj is HashMap<*, *>) {
                        val item = obj as HashMap<String, Any>
                        item[EventKey.EVENT_ID.key] = eventId
                        sendDeviceEvent(EventTable.WISHLIST_EVENTS_DETAIL.tableName, item)
                    }
                }
            }
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    fun addToWishList(eventDetails: HashMap<String, Any>) {
        sendWishListEvents(
            eventDetails = eventDetails,
            eventType = EventType.ADD.type
        )
    }

    internal fun removeFromWishList(eventDetails: HashMap<String, Any>) {
        sendWishListEvents(
            eventDetails = eventDetails,
            eventType = EventType.REMOVE.type
        )
    }

    internal fun sendCustomEvent(
        tableName: String,
        key: String,
        eventDetails: HashMap<String, Any>
    ) {
        try {
            DengageLogger.verbose("sendCustomEvent method is called")

            val sessionId = SessionManager.getSessionId()
            eventDetails[EventKey.SESSION_ID.key] = sessionId

            presenter.sendEvent(
                accountId = Prefs.sdkParameters!!.accountId,
                integrationKey = Prefs.subscription!!.integrationKey,
                key = key,
                eventTableName = tableName,
                eventDetails = eventDetails
            )

    } catch (e: Exception) {
        DengageLogger.error(e.message)
    }
    }

    internal fun sendDeviceEvent(
        tableName: String,
        eventDetails: HashMap<String, Any>
    ) {
        try {
            DengageLogger.verbose("sendDeviceEvent method is called")
            sendCustomEvent(
                tableName = tableName,
                key = Prefs.subscription?.getSafeDeviceId()!!,
                eventDetails = eventDetails
            )

    } catch (e: Exception) {
        DengageLogger.error(e.message)
    }
    }

    fun sendTransactionalOpenEvent(
        buttonId: String?,
        itemId: String?,
        messageId: Int?,
        messageDetails: String?,
        transactionId: String?
    ) {
        presenter.sendTransactionalOpenEvent(
            buttonId = buttonId,
            itemId = itemId,
            messageId = messageId,
            messageDetails = messageDetails,
            transactionId = transactionId,
            integrationKey = Prefs.subscription!!.integrationKey
        )
    }

    fun sendOpenEvent(
        buttonId: String?,
        itemId: String?,
        messageId: Int?,
        messageDetails: String?
    ) {
        presenter.sendOpenEvent(
            buttonId = buttonId,
            itemId = itemId,
            messageId = messageId,
            messageDetails = messageDetails,
            integrationKey = Prefs.subscription!!.integrationKey
        )
    }

    fun sendLoginEvent() {
        try {
            val subscription = Prefs.subscription
            val data = HashMap<String, Any>()
            val sessionId = SessionManager.getSessionId()

            data[EventKey.EVENT_NAME.key] = EventType.LOGIN.type
            subscription?.contactKey?.let { data.put(EventType.CONTACT_KEY.type, it) }
            data[EventKey.SESSION_ID.key] = sessionId

            presenter.sendEvent(
                accountId = Prefs.sdkParameters?.accountId,
                integrationKey = subscription!!.integrationKey,
                key = subscription.deviceId,
                eventTableName = EventTable.CUSTOM_ANALYTICS_EVENTS.tableName,
                eventDetails = data
            )
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    fun sendLogoutEvent() {
        try {
            val subscription = Prefs.subscription
            val data = HashMap<String, Any>()
            val sessionId = SessionManager.getSessionId()

            data[EventKey.EVENT_NAME.key] = EventType.LOGOUT.type
            subscription?.contactKey?.let { data.put(EventType.CONTACT_KEY.type, it) }
            data[EventKey.SESSION_ID.key] = sessionId

            presenter.sendEvent(
                accountId = Prefs.sdkParameters?.accountId,
                integrationKey = subscription!!.integrationKey,
                key = subscription.deviceId,
                eventTableName = EventTable.CUSTOM_ANALYTICS_EVENTS.tableName,
                eventDetails = data
            )
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    fun sendRegisterEvent() {
        try {
            val subscription = Prefs.subscription
            val data = HashMap<String, Any>()
            val sessionId = SessionManager.getSessionId()

            data[EventKey.EVENT_NAME.key] = EventType.REGISTER.type
            data[EventKey.SESSION_ID.key] = sessionId

            presenter.sendEvent(
                accountId = Prefs.sdkParameters?.accountId,
                integrationKey = subscription!!.integrationKey,
                key = subscription.deviceId,
                eventTableName = EventTable.CUSTOM_ANALYTICS_EVENTS.tableName,
                eventDetails = data
            )
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    override fun eventSent() = Unit
    override fun transactionalOpenEventSent() = Unit
    override fun openEventSent() = Unit
}