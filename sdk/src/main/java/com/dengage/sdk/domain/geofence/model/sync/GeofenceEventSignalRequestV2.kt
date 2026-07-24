package com.dengage.sdk.domain.geofence.model.sync

import com.dengage.sdk.util.Constants
import com.google.gson.annotations.SerializedName
import java.io.Serializable
import java.text.SimpleDateFormat
import java.util.Date
import java.util.Locale
import java.util.TimeZone

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
    @SerializedName("accuracyM") val accuracyM: Double? = null,
    @SerializedName("occurredAt") val occurredAt: String,
    @SerializedName("ingestedAt") val ingestedAt: String,
    @SerializedName("idempotencyKey") val idempotencyKey: String,
    @SerializedName("source") val source: GeofenceEventSource = GeofenceEventSource.ONLINE,
    /** Geçişi OS mu bildirdi (false) yoksa SDK mı çıkarsadı (true) — doc 22 §2.1. */
    @SerializedName("syntheticTransition") val syntheticTransition: Boolean = false,
    /**
     * Push token: backend'in geçişi push'lanabilir bir cihaza bağlaması için. Gönderim anında
     * subscription'dan taze çözülür (per-event saklanmaz) ki offline replay bayat token göndermesin.
     */
    @SerializedName("token") val token: String? = null
) : Serializable {

    companion object {
        // UTC + `XXX` pattern → sıfır offset `Z` olarak render edilir (iOS ile birebir: `2026-07-03T12:43:15Z`).
        private fun isoFormatter(): SimpleDateFormat =
            SimpleDateFormat(Constants.GEOFENCE_ISO_DATE_FORMAT, Locale.US).apply {
                timeZone = TimeZone.getTimeZone("UTC")
            }

        fun isoNow(): String = isoFormatter().format(Date())

        fun iso(millis: Long): String = isoFormatter().format(Date(millis))
    }
}
