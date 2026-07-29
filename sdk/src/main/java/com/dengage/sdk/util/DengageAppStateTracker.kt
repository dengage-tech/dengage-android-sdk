package com.dengage.sdk.util

import android.app.Activity
import android.app.Application
import android.content.Context
import android.os.Bundle

/**
 * Uygulamanın push ile arka planda uyandırılıp uyandırılmadığını ve gerçekten önde olup olmadığını
 * izler.
 *
 * Silent push (ör. `sourceType=geofence`) uygulama process'ini arka planda ayağa kaldırır; host
 * uygulamanın `Application.onCreate` -> `Dengage.init` akışı çalışır ve normal şartlarda in-app
 * fetch tetiklenir. Kullanıcı ekranda olmadığı için bu fetch hem gereksiz bir istektir hem de
 * fetch interval'ını yaktığı için kullanıcı uygulamayı gerçekten açtığında in-app gelmemesine
 * yol açar.
 *
 * [DengageUtils.isAppInForeground] tek başına yetmez: yüksek öncelikli bir FCM mesajı işlenirken
 * process geçici olarak `IMPORTANCE_FOREGROUND` seviyesine çıkar, yani ekranda hiçbir Activity
 * yokken bile "foreground" görünür. Bu yüzden [install] ile SDK kendi Activity lifecycle
 * callback'ini kaydeder ve Activity sayacını otoriter kaynak olarak kullanır. Kayıt yapılamadıysa
 * (ör. `Dengage.init` hiç çağrılmadıysa) eski davranışa düşülür.
 */
object DengageAppStateTracker {

    @Volatile
    private var startedActivityCount = 0

    @Volatile
    private var installed = false

    @Volatile
    private var wokenByBackgroundPush = false

    private val lifecycleCallbacks = object : Application.ActivityLifecycleCallbacks {
        override fun onActivityCreated(activity: Activity, savedInstanceState: Bundle?) = Unit

        override fun onActivityStarted(activity: Activity) = onActivityEnteredForeground()

        override fun onActivityResumed(activity: Activity) {
            // Geç install edildiysek (ör. Dengage.init bir Activity içinden çağrıldıysa) onStart'ı
            // kaçırmış olabiliriz; sayacı burada onar.
            if (startedActivityCount == 0) onActivityEnteredForeground()
        }

        override fun onActivityPaused(activity: Activity) = Unit

        override fun onActivityStopped(activity: Activity) = onActivityLeftForeground()

        override fun onActivitySaveInstanceState(activity: Activity, outState: Bundle) = Unit

        override fun onActivityDestroyed(activity: Activity) = Unit
    }

    /** [ContextHolder]'daki application context ile kurulum dener; context yoksa sessizce no-op. */
    fun install() {
        if (installed) return
        val context = try {
            ContextHolder.context
        } catch (_: Throwable) {
            null
        }
        install(context)
    }

    /** SDK'nın kendi Activity lifecycle takibini kurar. Birden fazla çağrıda tek kez uygulanır. */
    fun install(context: Context?) {
        if (installed) return
        val application = context?.applicationContext as? Application ?: return
        synchronized(this) {
            if (installed) return
            application.registerActivityLifecycleCallbacks(lifecycleCallbacks)
            installed = true
        }
    }

    @Synchronized
    private fun onActivityEnteredForeground() {
        startedActivityCount++
        wokenByBackgroundPush = false
    }

    @Synchronized
    private fun onActivityLeftForeground() {
        if (startedActivityCount > 0) startedActivityCount--
    }

    /** Ekranda görünür bir Activity var mı. */
    fun isInForeground(): Boolean =
        if (installed) startedActivityCount > 0
        else DengageUtils.isAppInForeground()

    /**
     * Push process'i arka planda uyandırdıysa işaretle. Uygulama zaten öndeyse no-op — kullanıcı
     * ekranda olduğu için in-app akışı normal çalışmalı.
     */
    fun markBackgroundPushWake() {
        if (isInForeground()) return
        if (wokenByBackgroundPush) return
        wokenByBackgroundPush = true
        DengageLogger.debug("DengageAppStateTracker -> background push wake, in app fetch suppressed")
    }

    /**
     * Push ile arka planda uyanıldı ve uygulama hâlâ arka planda mı. true ise in-app mesajları
     * çekilmemelidir.
     */
    fun shouldSkipInAppFetch(): Boolean {
        if (!wokenByBackgroundPush) return false
        if (isInForeground()) {
            wokenByBackgroundPush = false
            return false
        }
        return true
    }
}
