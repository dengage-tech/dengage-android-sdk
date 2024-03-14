package com.dengage.sdk.data.cache

import android.content.Context
import android.content.SharedPreferences
import com.dengage.sdk.data.remote.api.NotificationDisplayPriorityConfiguration
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.configuration.model.VisitorInfo
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.domain.rfm.model.RFMScore
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.visitcount.model.VisitCountItem
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.util.DengageUtils

object Prefs {

    internal val preferences: SharedPreferences by lazy {
        ContextHolder.context.getSharedPreferences(Constants.PREFS_NAME, Context.MODE_PRIVATE)
    }

    internal var installationId: String?
        get() = preferences.get(PreferenceKey.___DEN_DEVICE_UNIQUE_ID___)
        set(value) = preferences.set(PreferenceKey.___DEN_DEVICE_UNIQUE_ID___, value)

    internal var pushApiBaseUrl: String
        get() = preferences.get(PreferenceKey.PUSH_API_BASE_URL) ?: Constants.PUSH_API_URI
        set(value) = preferences.set(PreferenceKey.PUSH_API_BASE_URL, value)

    internal var eventApiBaseUrl: String
        get() = preferences.get(PreferenceKey.EVENT_API_BASE_URL) ?: Constants.EVENT_API_URI
        set(value) = preferences.set(PreferenceKey.EVENT_API_BASE_URL, value)

    internal var inAppApiBaseUrl: String
        get() = preferences.get(PreferenceKey.IN_APP_API_BASE_URL) ?: Constants.PUSH_API_URI
        set(value) = preferences.set(PreferenceKey.IN_APP_API_BASE_URL, value)

    internal var sdkParameters: SdkParameters?
        get() = preferences.get(PreferenceKey.SDK_PARAMETERS)
        set(value) = preferences.set(PreferenceKey.SDK_PARAMETERS, value)

    internal var appTrackingTime: Long
        get() = preferences.get(PreferenceKey.APP_TRACKING_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.APP_TRACKING_TIME, value)

    internal var inAppMessages: MutableList<InAppMessage>?
        get() = preferences.get(PreferenceKey.IN_APP_MESSAGES) ?: mutableListOf()
        set(value) = preferences.set(PreferenceKey.IN_APP_MESSAGES, value)

    internal var inAppMessageFetchTime: Long
        get() = preferences.get(PreferenceKey.IN_APP_MESSAGE_FETCH_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.IN_APP_MESSAGE_FETCH_TIME, value)

    internal var inAppRemoveFetchTime: Long
        get() = preferences.get(PreferenceKey.IN_APP_REMOVE_MESSAGE_FETCH_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.IN_APP_REMOVE_MESSAGE_FETCH_TIME, value)


    internal var realTimeInAppMessageFetchTime: Long
        get() = preferences.get(PreferenceKey.REAL_TIME_IN_APP_MESSAGE_FETCH_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.REAL_TIME_IN_APP_MESSAGE_FETCH_TIME, value)

    internal var inAppMessageShowTime: Long
        get() = preferences.get(PreferenceKey.IN_APP_MESSAGE_SHOW_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.IN_APP_MESSAGE_SHOW_TIME, value)

    internal var notificationChannelName: String
        get() = preferences.get(PreferenceKey.NOTIFICATION_CHANNEL_NAME,
            Constants.NOTIFICATION_CHANNEL_NAME)
            ?: Constants.NOTIFICATION_CHANNEL_NAME
        set(value) = preferences.set(PreferenceKey.NOTIFICATION_CHANNEL_NAME, value)

    internal var subscription: Subscription?
        get() = preferences.get(PreferenceKey.SUBSCRIPTION)
        set(value) = preferences.set(PreferenceKey.SUBSCRIPTION, value)

    internal var inboxMessageFetchTime: Long
        get() = preferences.get(PreferenceKey.INBOX_MESSAGE_FETCH_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.INBOX_MESSAGE_FETCH_TIME, value)

    internal var appSessionTime: Long
        get() = preferences.get(PreferenceKey.APP_SESSION_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.APP_SESSION_TIME, value)

    internal var appSessionId: String
        get() = preferences.get(PreferenceKey.APP_SESSION_ID, DengageUtils.generateUUID()) ?: ""
        set(value) = preferences.set(PreferenceKey.APP_SESSION_ID, value)

    internal var logVisibility: Boolean
        get() = preferences.get(PreferenceKey.LOG_VISIBILITY, false) ?: false
        set(value) = preferences.set(PreferenceKey.LOG_VISIBILITY, value)

    internal var rfmScores: MutableList<RFMScore>?
        get() = preferences.get(PreferenceKey.RFM_SCORES) ?: mutableListOf()
        set(value) = preferences.set(PreferenceKey.RFM_SCORES, value)

    internal var subscriptionCallTime: Long
        get() = preferences.get(PreferenceKey.SUBSCRIPTION_CALL_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.SUBSCRIPTION_CALL_TIME, value)

    internal var previouSubscription: Subscription?
        get() = preferences.get(PreferenceKey.PREVIOUS_SUBSCRIPTION)
        set(value) = preferences.set(PreferenceKey.PREVIOUS_SUBSCRIPTION, value)


    internal var firstLaunchTime: Long
        get() = preferences.get(PreferenceKey.FIRST_LAUNCH_TIME, 0L) ?: 0L
        set(value) = preferences.set(PreferenceKey.FIRST_LAUNCH_TIME, value)

    internal var lastSessionStartTime: Long
        get() = preferences.get(PreferenceKey.LAST_SESSION_START_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.LAST_SESSION_START_TIME, value)

    internal var lastSessionDuration: Long
        get() = preferences.get(PreferenceKey.LAST_SESSION_DURATION, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.LAST_SESSION_DURATION, value)

    internal var lastSessionVisitTime: Long
        get() = preferences.get(PreferenceKey.LAST_SESSION_VISIT_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.LAST_SESSION_VISIT_TIME, value)

    internal var visitCountItems: MutableList<VisitCountItem>
        get() = preferences.get(PreferenceKey.VISIT_COUNTS) ?: mutableListOf()
        set(value) = preferences.set(PreferenceKey.VISIT_COUNTS, value)

    internal var visitorInfo: VisitorInfo?
        get() = preferences.get(PreferenceKey.VISITOR_INFO) ?: VisitorInfo()
        set(value) = preferences.set(PreferenceKey.VISITOR_INFO, value)

    internal var inAppDeeplink: String
        get() = preferences.get(PreferenceKey.INAPP_DEEPLINK, "") ?: ""
        set(value) = preferences.set(PreferenceKey.INAPP_DEEPLINK, value)

    internal var lastPushPayload: Message?
        get() = preferences.get(PreferenceKey.LAST_MESSAGE_PUSH_PAYLOAD, null)
        set(value) = preferences.set(PreferenceKey.LAST_MESSAGE_PUSH_PAYLOAD, value)

    internal var restartApplicationAfterPushClick: Boolean?
        get() = preferences.get(PreferenceKey.RESTART_APPLICATION_AFTER_PUSH_CLICK, true)
        set(value) = preferences.set(PreferenceKey.RESTART_APPLICATION_AFTER_PUSH_CLICK, value)

    internal var isDevelopmentStatusDebug: Boolean?
        get() = preferences.get(PreferenceKey.DEVELOPMENT_STATUS, false)
        set(value) = preferences.set(PreferenceKey.DEVELOPMENT_STATUS, value)

    internal var visitorInfoFetchTime: Long
        get() = preferences.get(PreferenceKey.VISITOR_INFO_FETCH_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.VISITOR_INFO_FETCH_TIME, value)

    internal var className: String
        get() = preferences.get(PreferenceKey.CLASS_NAME, "") ?: ""
        set(value) = preferences.set(PreferenceKey.CLASS_NAME, value)

    internal var getRealTimeMessagesBaseUrl: String
        get() = preferences.get(PreferenceKey.REAL_TIME_IN_APP_API_BASE_URL)
            ?: Constants.GET_REAL_INAPP_MESSAGES_API_URI
        set(value) = preferences.set(PreferenceKey.REAL_TIME_IN_APP_API_BASE_URL, value)

    internal var disableOpenWebUrl: Boolean?
        get() = preferences.get(PreferenceKey.DISABLE_WEB_OPEN_URL) ?: false
        set (value) = preferences.set(PreferenceKey.DISABLE_WEB_OPEN_URL, value)

    internal var notificationDisplayPriorityConfiguration: Int?
        get() = preferences.get(PreferenceKey.NOTIFICATION_DISPLAY_PRIORITY_CONFIGURATION) ?: NotificationDisplayPriorityConfiguration.SHOW_WITH_DEFAULT_PRIORITY.ordinal
        set (value) = preferences.set(PreferenceKey.NOTIFICATION_DISPLAY_PRIORITY_CONFIGURATION, value)

    fun clear() {
        preferences.edit().clear().apply()
    }
}