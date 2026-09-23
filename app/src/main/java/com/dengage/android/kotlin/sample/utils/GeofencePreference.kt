package com.dengage.android.kotlin.sample.utils

import android.content.Context

/**
 * Kullanıcının geofence'i açık/kapalı tercihi. SDK `stopGeofence` sonrası `startGeofence` çağrısını
 * geofence'i yeniden açma isteği sayar. `Application.onCreate` her process başlangıcında (FCM silent
 * push, boot, WorkManager, geofence broadcast'leri dahil) çalıştığı için orada koşulsuz
 * `startGeofence` çağırmak kullanıcının durdurma kararını geri alır. Tercih burada saklanır.
 */
object GeofencePreference {
    private const val PREFS_FILE = "sample_app_prefs"
    private const val KEY_ENABLED = "geofence_enabled"

    fun isEnabled(context: Context): Boolean =
        context.getSharedPreferences(PREFS_FILE, Context.MODE_PRIVATE).getBoolean(KEY_ENABLED, true)

    fun setEnabled(context: Context, enabled: Boolean) {
        context.getSharedPreferences(PREFS_FILE, Context.MODE_PRIVATE)
            .edit().putBoolean(KEY_ENABLED, enabled).apply()
    }
}
