package com.dengage.sdk.domain.geofence.model.sync

import com.dengage.sdk.util.Constants
import com.google.gson.annotations.SerializedName
import java.io.Serializable
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import java.util.TimeZone

/**
 * `POST /devices/heartbeat/{integrationKey}` body (contract §2). `device_last_location` UPSERT besler.
 */
data class DeviceHeartbeatRequest(
    @SerializedName("deviceId") val deviceId: String,
    @SerializedName("contactKey") val contactKey: String? = null,
    @SerializedName("latitude") val latitude: Double,
    @SerializedName("longitude") val longitude: Double,
    @SerializedName("accuracyM") val accuracyM: Float? = null,
    @SerializedName("capturedAt") val capturedAt: String
) : Serializable {

    constructor(
        deviceId: String,
        contactKey: String?,
        latitude: Double,
        longitude: Double,
        accuracyM: Float?,
        capturedAtMillis: Long
    ) : this(
        deviceId = deviceId,
        contactKey = contactKey,
        latitude = latitude,
        longitude = longitude,
        accuracyM = accuracyM,
        capturedAt = SimpleDateFormat(Constants.GEOFENCE_ISO_DATE_FORMAT, Locale.US)
            // UTC → sıfır offset `Z` olarak render edilir (iOS ile birebir: `2026-07-03T12:43:15Z`).
            .apply { timeZone = TimeZone.getTimeZone("UTC") }
            .format(Date(capturedAtMillis))
    )
}
