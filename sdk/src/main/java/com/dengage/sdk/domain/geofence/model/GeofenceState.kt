package com.dengage.sdk.domain.geofence.model

import android.content.Context
import android.content.SharedPreferences
import android.location.Location
import androidx.core.content.edit
import com.dengage.sdk.data.cache.PreferenceKey
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.data.cache.get
import com.dengage.sdk.data.cache.set
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.ContextHolder

object GeofenceState {

    private const val KEY_LAST_LOCATION_LATITUDE = "last_location_latitude"
    private const val KEY_LAST_LOCATION_LONGITUDE = "last_location_longitude"
    private const val KEY_LAST_LOCATION_ACCURACY = "last_location_accuracy"
    private const val KEY_LAST_LOCATION_PROVIDER = "last_location_provider"
    private const val KEY_LAST_LOCATION_TIME = "last_location_time"
    private const val KEY_LAST_MOVED_LOCATION_LATITUDE = "last_moved_location_latitude"
    private const val KEY_LAST_MOVED_LOCATION_LONGITUDE = "last_moved_location_longitude"
    private const val KEY_LAST_MOVED_LOCATION_ACCURACY = "last_moved_location_accuracy"
    private const val KEY_LAST_MOVED_LOCATION_PROVIDER = "last_moved_location_provider"
    private const val KEY_LAST_MOVED_LOCATION_TIME = "last_moved_location_time"
    private const val KEY_LAST_MOVED_AT = "last_moved_at"
    private const val KEY_STOPPED = "stopped"
    private const val KEY_LAST_SENT_AT = "last_sent_at"
    private const val KEY_LAST_FAILED_STOPPED_LOCATION_LATITUDE = "last_failed_stopped_location_latitude"
    private const val KEY_LAST_FAILED_STOPPED_LOCATION_LONGITUDE = "last_failed_stopped_location_longitude"
    private const val KEY_LAST_FAILED_STOPPED_LOCATION_ACCURACY = "last_failed_stopped_location_accuracy"
    private const val KEY_LAST_FAILED_STOPPED_LOCATION_PROVIDER = "last_failed_stopped_location_provider"
    private const val KEY_LAST_FAILED_STOPPED_LOCATION_TIME = "last_failed_stopped_location_time"

    internal var lastLocationLatitude: Float
        get() = Prefs.preferences.get(KEY_LAST_LOCATION_LATITUDE, 0f) ?: 0f
        set(value) = Prefs.preferences.set(KEY_LAST_LOCATION_LATITUDE, value)

    internal var lastLocationLongitude: Float
        get() = Prefs.preferences.get(KEY_LAST_LOCATION_LONGITUDE, 0f) ?: 0f
        set(value) = Prefs.preferences.set(KEY_LAST_LOCATION_LONGITUDE, value)

    internal var lastLocationAccuracy: Float
        get() = Prefs.preferences.get(KEY_LAST_LOCATION_ACCURACY, 0f) ?: 0f
        set(value) = Prefs.preferences.set(KEY_LAST_LOCATION_ACCURACY, value)

    internal var lastLocationProvider: String
        get() = Prefs.preferences.get(KEY_LAST_LOCATION_PROVIDER) ?: "DengageSDK"
        set(value) = Prefs.preferences.set(KEY_LAST_LOCATION_PROVIDER, value)

    internal var lastLocationTime: Long
        get() = Prefs.preferences.get(KEY_LAST_LOCATION_TIME, 0) ?: 0
        set(value) = Prefs.preferences.set(KEY_LAST_LOCATION_TIME, value)

    internal var lastMovedLocationLatitude: Float
        get() = Prefs.preferences.get(KEY_LAST_MOVED_LOCATION_LATITUDE, 0f) ?: 0f
        set(value) = Prefs.preferences.set(KEY_LAST_MOVED_LOCATION_LATITUDE, value)

    internal var lastMovedLocationLongitude: Float
        get() = Prefs.preferences.get(KEY_LAST_MOVED_LOCATION_LONGITUDE, 0f) ?: 0f
        set(value) = Prefs.preferences.set(KEY_LAST_MOVED_LOCATION_LONGITUDE, value)

    internal var lastMovedLocationAccuracy: Float
        get() = Prefs.preferences.get(KEY_LAST_MOVED_LOCATION_ACCURACY, 0f) ?: 0f
        set(value) = Prefs.preferences.set(KEY_LAST_MOVED_LOCATION_ACCURACY, value)

    internal var lastMovedLocationProvider: String
        get() = Prefs.preferences.get(KEY_LAST_MOVED_LOCATION_PROVIDER) ?: "DengageSDK"
        set(value) = Prefs.preferences.set(KEY_LAST_MOVED_LOCATION_PROVIDER, value)

    internal var lastMovedLocationTime: Long
        get() = Prefs.preferences.get(KEY_LAST_MOVED_LOCATION_TIME, 0) ?: 0
        set(value) = Prefs.preferences.set(KEY_LAST_MOVED_LOCATION_TIME, value)

    internal var lastFailedStoppedLocationLatitude: Float
        get() = Prefs.preferences.get(KEY_LAST_FAILED_STOPPED_LOCATION_LATITUDE, 0f) ?: 0f
        set(value) = Prefs.preferences.set(KEY_LAST_FAILED_STOPPED_LOCATION_LATITUDE, value)

    internal var lastFailedStoppedLocationLongitude: Float
        get() = Prefs.preferences.get(KEY_LAST_FAILED_STOPPED_LOCATION_LONGITUDE, 0f) ?: 0f
        set(value) = Prefs.preferences.set(KEY_LAST_FAILED_STOPPED_LOCATION_LONGITUDE, value)

    internal var lastFailedStoppedLocationAccuracy: Float
        get() = Prefs.preferences.get(KEY_LAST_FAILED_STOPPED_LOCATION_ACCURACY, 0f) ?: 0f
        set(value) = Prefs.preferences.set(KEY_LAST_FAILED_STOPPED_LOCATION_ACCURACY, value)

    internal var lastFailedStoppedLocationProvider: String
        get() = Prefs.preferences.get(KEY_LAST_FAILED_STOPPED_LOCATION_PROVIDER) ?: "DengageSDK"
        set(value) = Prefs.preferences.set(KEY_LAST_FAILED_STOPPED_LOCATION_PROVIDER, value)

    internal var lastFailedStoppedLocationTime: Long
        get() = Prefs.preferences.get(KEY_LAST_FAILED_STOPPED_LOCATION_TIME, 0) ?: 0
        set(value) = Prefs.preferences.set(KEY_LAST_FAILED_STOPPED_LOCATION_TIME, value)

    var lastMovedAt: Long
        get() = Prefs.preferences.get(KEY_LAST_MOVED_AT, 0) ?: 0
        set(value) = Prefs.preferences.set(KEY_LAST_MOVED_AT, value)

    var stopped: Boolean
        get() = Prefs.preferences.get(KEY_STOPPED, false) ?: false
        set(value) = Prefs.preferences.set(KEY_STOPPED, value)

    var lastSentAt: Long
        get() = Prefs.preferences.get(KEY_LAST_SENT_AT, 0) ?: 0
        set(value) = Prefs.preferences.set(KEY_LAST_SENT_AT, value)

    fun valid(location: Location): Boolean {
        val latitudeValid = location.latitude != 0.0 && location.latitude > -90.0 && location.latitude < 90.0
        val longitudeValid = location.longitude != 0.0 && location.longitude > -180.0 && location.longitude < 180.0
        val accuracyValid = location.accuracy > 0f
        return latitudeValid && longitudeValid && accuracyValid
    }

    fun getLastLocation(): Location? {
        val location = Location(lastLocationProvider)
        location.latitude = lastLocationLatitude.toDouble()
        location.longitude = lastLocationLongitude.toDouble()
        location.accuracy = lastLocationAccuracy
        location.time = lastLocationTime
        if (!valid(location)) {
            return null
        }
        return location
    }

    fun setLastLocation(location: Location?) {
        if (location == null || !valid(location)) {
            Prefs.preferences.edit {
                remove(KEY_LAST_LOCATION_LATITUDE)
                remove(KEY_LAST_LOCATION_LONGITUDE)
                remove(KEY_LAST_LOCATION_ACCURACY)
                remove(KEY_LAST_LOCATION_PROVIDER)
                remove(KEY_LAST_LOCATION_TIME)
            }
            return
        }
        lastLocationLatitude = location.latitude.toFloat()
        lastLocationLongitude = location.longitude.toFloat()
        lastLocationAccuracy = location.accuracy
        lastLocationProvider = location.provider.toString()
        lastLocationTime = location.time
    }

    fun getLastMovedLocation(): Location? {
        val location = Location(lastMovedLocationProvider)
        location.latitude = lastMovedLocationLatitude.toDouble()
        location.longitude = lastMovedLocationLongitude.toDouble()
        location.accuracy = lastMovedLocationAccuracy
        location.time = lastMovedLocationTime
        if (!valid(location)) {
            return null
        }
        return location
    }

    fun setLastMovedLocation(location: Location?) {
        if (location == null || !valid(location)) {
            return
        }
        lastMovedLocationLatitude = location.latitude.toFloat()
        lastMovedLocationLongitude = location.longitude.toFloat()
        lastMovedLocationAccuracy = location.accuracy
        lastMovedLocationProvider = location.provider.toString()
        lastMovedLocationTime = location.time
    }

    fun getLastFailedStoppedLocation(): Location? {
        val location = Location(lastFailedStoppedLocationProvider)
        location.latitude = lastFailedStoppedLocationLatitude.toDouble()
        location.longitude = lastFailedStoppedLocationLongitude.toDouble()
        location.accuracy = lastFailedStoppedLocationAccuracy
        location.time = lastFailedStoppedLocationTime
        if (!valid(location)) {
            return null
        }
        return location
    }

    fun setLastFailedStoppedLocation(location: Location?) {
        if (location == null || !valid(location)) {
            Prefs.preferences.edit {
                remove(KEY_LAST_FAILED_STOPPED_LOCATION_LATITUDE)
                remove(KEY_LAST_FAILED_STOPPED_LOCATION_LONGITUDE)
                remove(KEY_LAST_FAILED_STOPPED_LOCATION_ACCURACY)
                remove(KEY_LAST_FAILED_STOPPED_LOCATION_PROVIDER)
                remove(KEY_LAST_FAILED_STOPPED_LOCATION_TIME)
            }
            return
        }
        lastFailedStoppedLocationLatitude = location.latitude.toFloat()
        lastFailedStoppedLocationLongitude = location.longitude.toFloat()
        lastFailedStoppedLocationAccuracy = location.accuracy
        lastFailedStoppedLocationProvider = location.provider.toString()
        lastFailedStoppedLocationTime = location.time
    }

}