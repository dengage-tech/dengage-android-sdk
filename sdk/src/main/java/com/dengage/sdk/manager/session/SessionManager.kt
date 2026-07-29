package com.dengage.sdk.manager.session

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.manager.inappmessage.util.RealTimeInAppParamHolder
import com.dengage.sdk.manager.visitcount.VisitCountManager
import com.dengage.sdk.util.DengageUtils
import java.util.concurrent.TimeUnit

/**
 * Oturum (session) yönetimi.
 *
 * İki erişim biçimi vardır ve karıştırılmamalıdır:
 *
 * - [getSessionId] **kullanıcı aktivitesi** noktalarında kullanılır (event gönderimi, uygulama öne
 *   gelmesi, in-app etkileşimi). Süresi dolmuşsa yeni oturum açar, dolmamışsa kayan pencereyi
 *   ileri taşır.
 * - [currentSessionId] **salt okumadır**; oturum döndürmez, süresini uzatmaz, ziyaret saymaz.
 *   Kural değerlendirmesi, debug log'u gibi yan etkisi olmaması gereken yollar bunu kullanır.
 */
object SessionManager {

    private const val DEFAULT_SESSION_TIMEOUT_MINUTES = 30

    /**
     * Kayan pencere yazma eşiği: kalan ömür, timeout'un bu oranından fazlaysa expiry yeniden
     * yazılmaz. [getSessionId] her event'te çağrıldığı için, aksi halde her çağrı bir
     * SharedPreferences yazması olurdu.
     */
    private const val SLIDE_WRITE_THRESHOLD = 0.9

    /**
     * Yürürlükteki oturum id'si, yan etkisiz. Oturumun süresi dolmuşsa boş string döner: süresi
     * dolmuş bir oturumun event'leri "bu oturumda" sayılmaz.
     */
    val currentSessionId: String
        get() = if (isExpired()) "" else Prefs.appSessionId

    /**
     * Kullanıcı aktivitesine bağlı oturum erişimi.
     *
     * @param force true ise mevcut oturum süresi dolmamış olsa da yeni oturum açar
     *        (ör. contact key değişimi).
     */
    @Synchronized
    fun getSessionId(force: Boolean = false): String {
        val timeoutMillis = sessionTimeoutMillis()

        if (force || isExpired()) {
            return startNewSession(timeoutMillis)
        }

        slideExpiryIfNeeded(timeoutMillis)
        return Prefs.appSessionId
    }

    private fun isExpired(): Boolean = System.currentTimeMillis() > Prefs.appSessionTime

    private fun startNewSession(timeoutMillis: Long): String {
        val sessionId = DengageUtils.generateUUID()
        Prefs.appSessionId = sessionId
        Prefs.appSessionTime = System.currentTimeMillis() + timeoutMillis

        VisitCountManager.updateVisitCount()
        RealTimeInAppParamHolder.pageViewVisitCount = 0
        return sessionId
    }

    /** Kayan pencere: son kullanma "şimdi + timeout" olur, ama her çağrıda diske yazılmaz. */
    private fun slideExpiryIfNeeded(timeoutMillis: Long) {
        if (timeoutMillis <= 0) return

        val remaining = Prefs.appSessionTime - System.currentTimeMillis()
        if (remaining > timeoutMillis * SLIDE_WRITE_THRESHOLD) return

        Prefs.appSessionTime = System.currentTimeMillis() + timeoutMillis
    }

    private fun sessionTimeoutMillis(): Long {
        val minutes = Prefs.sdkParameters?.realTimeInAppSessionTimeoutMinutes
            ?: DEFAULT_SESSION_TIMEOUT_MINUTES
        return TimeUnit.MINUTES.toMillis(minutes.toLong())
    }
}
