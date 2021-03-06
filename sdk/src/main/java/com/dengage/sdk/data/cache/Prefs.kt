package com.dengage.sdk.data.cache

import android.content.Context
import android.content.SharedPreferences
import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.geofence.model.GeofenceHistory
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.rfm.model.RFMScore
import com.dengage.sdk.domain.subscription.model.Subscription
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

    internal var sdkParameters: SdkParameters?
        get() = preferences.get(PreferenceKey.SDK_PARAMETERS)
        set(value) = preferences.set(PreferenceKey.SDK_PARAMETERS, value)

    internal var appTrackingTime: Long
        get() = preferences.get(PreferenceKey.APP_TRACKING_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.APP_TRACKING_TIME, value)

    internal var inAppMessages: MutableList<InAppMessage>?
        get() = preferences.get(PreferenceKey.IN_APP_MESSAGES)
        set(value) = preferences.set(PreferenceKey.IN_APP_MESSAGES, value)

    internal var inAppMessageFetchTime: Long
        get() = preferences.get(PreferenceKey.IN_APP_MESSAGE_FETCH_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.IN_APP_MESSAGE_FETCH_TIME, value)

    internal var inAppMessageShowTime: Long
        get() = preferences.get(PreferenceKey.IN_APP_MESSAGE_SHOW_TIME, 0) ?: 0
        set(value) = preferences.set(PreferenceKey.IN_APP_MESSAGE_SHOW_TIME, value)

    internal var notificationChannelName: String
        get() = preferences.get(PreferenceKey.NOTIFICATION_CHANNEL_NAME, Constants.NOTIFICATION_CHANNEL_NAME)
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
        get() = preferences.get(PreferenceKey.RFM_SCORES)
        set(value) = preferences.set(PreferenceKey.RFM_SCORES, value)

    internal var geofenceApiBaseUrl: String
        get() = preferences.get(PreferenceKey.GEOFENCE_API_BASE_URL) ?: Constants.GEOFENCE_API_URI
        set(value) = preferences.set(PreferenceKey.GEOFENCE_API_BASE_URL, value)

    internal var geofenceEnabled: Boolean
        get() = preferences.get(PreferenceKey.GEOFENCE_ENABLED, defaultValue = false) ?:false
        set(value) = preferences.set(PreferenceKey.GEOFENCE_ENABLED, value)

    internal var geofenceHistory: GeofenceHistory
        get() = preferences.get(PreferenceKey.GEOFENCE_HISTORY, defaultValue = GeofenceHistory()) ?: GeofenceHistory()
        set(value) = preferences.set(PreferenceKey.GEOFENCE_HISTORY, value)

    internal var geofencePermissionsDenied: Boolean
        get() = preferences.get(PreferenceKey.GEOFENCE_PERMISSIONS_DENIED, defaultValue = false) ?:false
        set(value) = preferences.set(PreferenceKey.GEOFENCE_PERMISSIONS_DENIED, value)

    fun clear() {
        preferences.edit().clear().apply()
    }
}