package com.dengage.sdk.domain.geofence.model.sync

import com.dengage.sdk.util.Constants
import com.google.gson.annotations.SerializedName
import java.io.Serializable
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale

/**
 * `POST /event-signal/{integrationKey}` v2 body (contract §3).
 * camelCase alanlar; v1'in kısa anahtarları (cid/geoid) kullanılmaz.
 * Offline replay için `occurredAt` (trigger anı) + dedup için `idempotencyKey` taşır.
 */
data class GeofenceEventSignalRequestV2(
    @SerializedName("deviceId") val deviceId: String,
    @SerializedName("contactKey") val contactKey: String? = null,
    @SerializedName("geofenceId") val geofenceId: Int,
    @SerializedName("clusterId") val clusterId: Int,
    @SerializedName("campaignId") val campaignId: Int? = null,
    @SerializedName("eventType") val eventType: GeofenceEventType,
    @SerializedName("latitude") val latitude: Double,
    @SerializedName("longitude") val longitude: Double,
    @SerializedName("occurredAt") val occurredAt: String,
    @SerializedName("ingestedAt") val ingestedAt: String,
    @SerializedName("idempotencyKey") val idempotencyKey: String,
    @SerializedName("source") val source: GeofenceEventSource = GeofenceEventSource.ONLINE
) : Serializable {

    companion object {
        fun isoNow(): String =
            SimpleDateFormat(Constants.GEOFENCE_ISO_DATE_FORMAT, Locale.US).format(Date())

        fun iso(millis: Long): String =
            SimpleDateFormat(Constants.GEOFENCE_ISO_DATE_FORMAT, Locale.US).format(Date(millis))
    }
}
