package com.dengage.sdk.manager.event

import java.util.*
import java.util.concurrent.TimeUnit
import androidx.core.net.toUri
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.event.model.FilterOperator
import com.dengage.sdk.domain.event.model.ClientEvent
import com.dengage.sdk.manager.base.BaseMvpManager
import com.dengage.sdk.manager.inappmessage.util.RealTimeInAppParamHolder
import com.dengage.sdk.manager.session.SessionManager
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.domain.configuration.model.EventTypeDefinition

class EventManager : BaseMvpManager<EventContract.View, EventContract.Presenter>(),
    EventContract.View {

    override fun providePresenter() = EventPresenter()

    private var isSessionStarted = false

    internal fun sessionStart(referer: String) {
        if (isSessionStarted) return
        try {
            val data = HashMap<String, Any>()
            try {
                val uri = referer.toUri()
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
            
            // Set page parameters for real-time in-app messages
            RealTimeInAppParamHolder.setClientPageInfo(eventDetails)
            
            sendDeviceEvent(EventTable.PAGE_VIEW_EVENTS.tableName, eventDetails)
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
                        product[EventKey.EVENT_TYPE.key] = eventType
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
                if (copyData[EventKey.TOTAL_AMOUNT.key].toString().toIntOrNull() != null) {
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
                            val toString = copyData[EventKey.ORDER_ID.key].toString()
                            product[EventKey.ORDER_ID.key] =
                                toString
                            product[EventKey.EVENT_TYPE.key] = EventType.CANCEL.type
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
                            product[EventKey.EVENT_TYPE.key] = EventType.ORDER.type
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

            if(tableName == EventTable.PAGE_VIEW_EVENTS.tableName){
                RealTimeInAppParamHolder.addPageView()
                // Set page parameters for page view events
                RealTimeInAppParamHolder.setClientPageInfo(eventDetails)
            }

            val subscription = Prefs.subscription

            val sessionId = SessionManager.getSessionId()
            eventDetails[EventKey.SESSION_ID.key] = sessionId


            subscription?.deviceId?.let { eventDetails.put(EventKey.DN_DEVICE_ID.key, it) }
            subscription?.contactKey?.let { eventDetails.put(EventKey.DN_CONTACT_KEY.key, it) }


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

    internal fun cleanupClientEvents() {
        try {
            DengageLogger.debug("cleanupClientEvents has been called")
            val currentTime = System.currentTimeMillis()
            val lastCleanupTime = Prefs.clientEventsLastCleanupTime
            
            // Only cleanup if it's been more than 10 minutes since last cleanup
            val cleanupInterval = 10 * 60 * 1000L // 10 minutes in milliseconds
            if (currentTime - lastCleanupTime < cleanupInterval) {
                return
            }
            
            val sdkParameters = Prefs.sdkParameters ?: return
            val clientEvents = Prefs.clientEvents
            var hasChanges = false
            
            // Get all valid event types from eventMappings
            val validEventTypes = sdkParameters.eventMappings
                ?.flatMap { it.eventTypeDefinitions ?: emptyList() }
                ?.mapNotNull { it.eventType }
                ?.toSet() ?: emptySet()
            
            // Remove events that are no longer in eventMappings
            val orphanedEventTypes = clientEvents.keys.filter { eventType ->
                !validEventTypes.contains(eventType)
            }
            
            orphanedEventTypes.forEach { eventType ->
                clientEvents.remove(eventType)
                hasChanges = true
                DengageLogger.debug("Removed orphaned event type: $eventType (not found in eventMappings)")
            }
            
            // Process remaining valid event types
            clientEvents.forEach { (eventType, eventTypeEvents) ->
                if (eventTypeEvents.isNotEmpty()) {
                    val matchingEventTypeDefinition = sdkParameters.eventMappings
                        ?.flatMap { it.eventTypeDefinitions ?: emptyList() }
                        ?.find { it.eventType == eventType }
                    
                    if (matchingEventTypeDefinition?.enableClientHistory == true) {
                        val clientHistoryOptions = matchingEventTypeDefinition.clientHistoryOptions
                        if (clientHistoryOptions != null) {
                            val maxEventCount = clientHistoryOptions.maxEventCount ?: Int.MAX_VALUE
                            val timeWindowInMinutes = clientHistoryOptions.timeWindowInMinutes ?: Int.MAX_VALUE
                            
                            val timeThreshold = currentTime - TimeUnit.MINUTES.toMillis(timeWindowInMinutes.toLong())
                            val filteredEvents = eventTypeEvents.filter { it.timestamp >= timeThreshold }.toMutableList()
                            
                            // Keep only the latest maxEventCount events
                            val finalEvents = if (filteredEvents.size > maxEventCount) {
                                filteredEvents.sortedByDescending { it.timestamp }.take(maxEventCount).toMutableList()
                            } else {
                                filteredEvents
                            }
                            
                            // Update if there are changes
                            if (finalEvents.size != eventTypeEvents.size) {
                                clientEvents[eventType] = finalEvents
                                hasChanges = true
                                DengageLogger.debug("Cleaned up events for type: $eventType, removed: ${eventTypeEvents.size - finalEvents.size} events")
                            }
                        }
                    } else {
                        // Remove events for event types that have enableClientHistory = false
                        clientEvents.remove(eventType)
                        hasChanges = true
                        DengageLogger.debug("Removed events for type: $eventType (client history disabled)")
                    }
                }
            }
            
            // Save changes and update cleanup time
            if (hasChanges) {
                Prefs.clientEvents = clientEvents
            }
            Prefs.clientEventsLastCleanupTime = currentTime
            
        } catch (e: Exception) {
            DengageLogger.error("Error cleaning up client events: ${e.message}")
        }
    }

    private fun transformEventDetailsKeys(
        matchingEventType: EventTypeDefinition,
        eventDetails: Map<String, Any>
    ): Map<String, Any> {
        try {
            // Get attributes for matching event type definition
            val allAttributes = matchingEventType.attributes ?: return eventDetails

            // Create mapping from tableColumnName to name
            val keyMappings = allAttributes.associate { attribute ->
                attribute.tableColumnName to attribute.name
            }.filterValues { it != null }.mapValues { it.value!! }

            // Transform the event details
            val transformedDetails = mutableMapOf<String, Any>()

            eventDetails.forEach { (key, value) ->
                val newKey = keyMappings[key] ?: key
                transformedDetails[newKey] = value
            }

            return transformedDetails
        } catch (e: Exception) {
            DengageLogger.error("Error transforming event details keys: ${e.message}")
            return eventDetails
        }
    }

    override fun eventSent(tableName: String, key: String?, eventDetails: Map<String, Any>) {
        try {
            val sdkParameters = Prefs.sdkParameters ?: return

            // Find matching event mapping
            val eventMapping = sdkParameters.eventMappings?.find { it.eventTableName == tableName } ?: return

            // Check event type definitions
            val eventTypeDefinitions = eventMapping.eventTypeDefinitions ?: return

            // Check if the event meets the criteria from eventTypeDefinitions
            val matchingEventType = eventTypeDefinitions.find { eventTypeDefinition ->
                // Check if client history is enabled for this event type definition
                if (eventTypeDefinition.enableClientHistory != true) return@find false

                // If there's only one eventTypeDefinition, skip filter condition check
                if (eventTypeDefinitions.size == 1) {
                    return@find true
                }

                // Check filter conditions
                val filterConditions = eventTypeDefinition.filterConditions
                if (filterConditions.isNullOrEmpty()) {
                    return@find true
                }

                val logicOperator = eventTypeDefinition.logicOperator ?: "AND"

                when (logicOperator) {
                    "AND" -> filterConditions.all { condition ->
                        checkFilterCondition(
                            condition.fieldName,
                            condition.operator,
                            condition.values,
                            eventDetails
                        )
                    }

                    "OR" -> filterConditions.any { condition ->
                        checkFilterCondition(
                            condition.fieldName,
                            condition.operator,
                            condition.values,
                            eventDetails
                        )
                    }

                    else -> false
                }
            }

            // If no matching event type definition, don't store the event
            if (matchingEventType == null || matchingEventType.eventType == null) return

            // Transform event details keys first
            val transformedEventDetails = transformEventDetailsKeys(matchingEventType, eventDetails)

            // Get the current client events for this table
            val clientEvents = Prefs.clientEvents
            val eventTypeEvents = clientEvents[matchingEventType.eventType] ?: mutableListOf()

            val clientEvent = ClientEvent(
                tableName = tableName,
                key = key,
                eventDetails = transformedEventDetails,
                timestamp = System.currentTimeMillis(),
                eventType = matchingEventType.eventType
            )

            eventTypeEvents.add(clientEvent)
            clientEvents[matchingEventType.eventType] = eventTypeEvents
            Prefs.clientEvents = clientEvents

            DengageLogger.debug("Client Event stored for table: $tableName for eventType: ${matchingEventType.eventType}, current count: ${eventTypeEvents.size}")
        } catch (e: Exception) {
            DengageLogger.error("Error storing event: ${e.message}")
        }
    }

    private fun checkFilterCondition(
        fieldName: String?,
        operatorValue: String?,
        values: List<String>?,
        eventDetails: Map<String, Any>
    ): Boolean {
        if (fieldName.isNullOrEmpty() || operatorValue == null) return true

        val fieldValue = eventDetails[fieldName]?.toString() ?: return false

        val operator = FilterOperator.fromValue(operatorValue) ?: return false

        return when (operator) {
            FilterOperator.EQUALS -> values?.firstOrNull() == fieldValue
            FilterOperator.NOT_EQUALS -> values?.firstOrNull() != fieldValue
            FilterOperator.IN -> values?.contains(fieldValue) == true
            FilterOperator.NOT_IN -> values?.contains(fieldValue) != true
            FilterOperator.LIKE -> values?.firstOrNull()
                ?.let { fieldValue.contains(it, ignoreCase = true) } ?: false

            FilterOperator.NOT_LIKE -> values?.firstOrNull()
                ?.let { !fieldValue.contains(it, ignoreCase = true) } ?: true

            FilterOperator.STARTS_WITH -> values?.firstOrNull()
                ?.let { fieldValue.startsWith(it, ignoreCase = true) } ?: false

            FilterOperator.NOT_STARTS_WITH -> values?.firstOrNull()
                ?.let { !fieldValue.startsWith(it, ignoreCase = true) } ?: true

            FilterOperator.ENDS_WITH -> values?.firstOrNull()
                ?.let { fieldValue.endsWith(it, ignoreCase = true) } ?: false

            FilterOperator.NOT_ENDS_WITH -> values?.firstOrNull()
                ?.let { !fieldValue.endsWith(it, ignoreCase = true) } ?: true

            FilterOperator.GREATER_THAN -> try {
                val numFieldValue = fieldValue.toDouble()
                values?.firstOrNull()?.toDouble()?.let { numFieldValue > it } ?: false
            } catch (e: Exception) {
                DengageLogger.error("Error checkFilterCondition: ${e.message}")
                false
            }

            FilterOperator.GREATER_EQUAL -> try {
                val numFieldValue = fieldValue.toDouble()
                values?.firstOrNull()?.toDouble()?.let { numFieldValue >= it } ?: false
            } catch (e: Exception) {
                DengageLogger.error("Error checkFilterCondition: ${e.message}")
                false
            }

            FilterOperator.LESS_THAN -> try {
                val numFieldValue = fieldValue.toDouble()
                values?.firstOrNull()?.toDouble()?.let { numFieldValue < it } ?: false
            } catch (e: Exception) {
                DengageLogger.error("Error checkFilterCondition: ${e.message}")
                false
            }

            FilterOperator.LESS_EQUAL -> try {
                val numFieldValue = fieldValue.toDouble()
                values?.firstOrNull()?.toDouble()?.let { numFieldValue <= it } ?: false
            } catch (e: Exception) {
                DengageLogger.error("Error checkFilterCondition: ${e.message}")
                false
            }

            FilterOperator.BETWEEN -> try {
                if ((values?.size ?: 0) < 2) return false
                val numFieldValue = fieldValue.toDouble()
                val min = values!![0].toDouble()
                val max = values[1].toDouble()
                numFieldValue in min..max
            } catch (e: Exception) {
                DengageLogger.error("Error checkFilterCondition: ${e.message}")
                false
            }

            FilterOperator.NOT_BETWEEN -> try {
                if ((values?.size ?: 0) < 2) return false
                val numFieldValue = fieldValue.toDouble()
                val min = values!![0].toDouble()
                val max = values[1].toDouble()
                numFieldValue !in min..max
            } catch (e: Exception) {
                DengageLogger.error("Error checkFilterCondition: ${e.message}")
                false
            }

            FilterOperator.NULL -> fieldValue == null || fieldValue.isEmpty()
            FilterOperator.NOT_NULL -> fieldValue != null && fieldValue.isNotEmpty()
            FilterOperator.EMPTY -> fieldValue.isEmpty()
            FilterOperator.NOT_EMPTY -> fieldValue.isNotEmpty()
            // Handle other operators as needed
            else -> false
        }
    }

    override fun transactionalOpenEventSent() = Unit
    override fun openEventSent() = Unit
}