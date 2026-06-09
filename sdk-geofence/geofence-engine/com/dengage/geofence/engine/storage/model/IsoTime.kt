package com.dengage.geofence.engine.storage.model

import com.dengage.sdk.util.Constants
import java.text.SimpleDateFormat
import java.util.Locale

/** ISO 8601 (offset'li) <-> epoch millis dönüşümü için yardımcı (contract §0). */
object IsoTime {

    // Server farklı ISO varyantları gönderebilir; parse'ta toleranslı ol.
    private val PARSE_PATTERNS = listOf(
        Constants.GEOFENCE_ISO_DATE_FORMAT,        // 2026-05-18T10:00:00+03:00
        "yyyy-MM-dd'T'HH:mm:ssXXX",
        "yyyy-MM-dd'T'HH:mm:ss.SSSXXX",            // milisaniyeli offset
        "yyyy-MM-dd'T'HH:mm:ssZ",                  // +0300 (kolonsuz)
        "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'",            // legacy UTC literal Z
        "yyyy-MM-dd'T'HH:mm:ss'Z'"
    )

    fun parseOrNull(iso: String?): Long? {
        if (iso.isNullOrBlank()) return null
        for (pattern in PARSE_PATTERNS) {
            try {
                return SimpleDateFormat(pattern, Locale.US).parse(iso)?.time ?: continue
            } catch (e: Exception) {
                // sonraki pattern'i dene
            }
        }
        return null
    }
}
