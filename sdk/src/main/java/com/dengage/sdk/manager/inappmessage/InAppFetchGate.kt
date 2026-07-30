package com.dengage.sdk.manager.inappmessage

import kotlin.math.max
import kotlin.math.min

/**
 * Uyarlamalı fetch gate'i.
 *
 * Sabit bir aralık, boş yanıt oranının %97 olduğu bir sistemde zamanın çoğunda gereksiz sık
 * demektir. Gate son yanıtlara bakarak kendini ayarlar: boş yanıtta kademeli geri çekilir, dolu
 * yanıtta tabana döner. Karar cihazda verilir; sunucuya sormaya gerek yok, çünkü boş/dolu bilgisi
 * zaten yanıtın kendisinde.
 *
 * - Taban: hesabın kanal bazlı fetch aralığı ayarı (sunucudan gelir, hesap bazında değiştirilebilir),
 *   en az [MIN_BASE_MS]. Ayar 0 gelirse çarpım 0'da kilitlenir ve geri çekilme hiç devreye girmez.
 * - Tavan: `max(taban, 15 dk)` — ayarını tavanın üstüne çekmiş bir hesapta geri çekilme, hesabın
 *   istediğinden daha sık fetch etmeye **dönüşmemeli**
 * - Sıfırlama: uygulama ön plana geldiğinde. Oturum başı çapası kayan pencereyle birlikte
 *   neredeyse hiç tetiklenmeyen bir olaya dönüştüğü için tercih edilmedi.
 *
 * Ayrıca kanal başına bir **uçuş bayrağı** tutar: damga yanıt sonrasında atıldığı için, istek
 * uçuştayken gelen ikinci bir tetikleyici aralık kontrolünü geçip aynı çağrıyı tekrar gönderebilir.
 *
 * Durum yalnızca bellekte tutulur: process ölürse zaten sıfırlanması gerekir.
 */
internal object InAppFetchGate {

    enum class Channel { BULK, REAL_TIME }

    private const val BACKOFF_MULTIPLIER = 1.2
    private const val CEILING_MS = 15 * 60 * 1000L

    /** Gate tabanının alt sınırı. Hesap ayarı 0 ya da tanımsızsa bu değer kullanılır. */
    private const val MIN_BASE_MS = 60_000L

    /**
     * Uçuş bayrağının kendiliğinden düşme süresi. Yanıt geri çağrısı hiç çalışmazsa kanalın
     * kalıcı olarak kilitlenmemesi için; istek zaman aşımının rahatça üstünde.
     */
    private const val IN_FLIGHT_TIMEOUT_MS = 30_000L

    private val gates = mutableMapOf<Channel, Long>()
    private val inFlightSince = mutableMapOf<Channel, Long>()

    /** Kanalın yürürlükteki gate'i (ms). */
    @Synchronized
    fun current(channel: Channel, baseMs: Long): Long = gates[channel] ?: effectiveBase(baseMs)

    /** Yanıt sonrası gate'i güncelle ve yeni değeri döndür. */
    @Synchronized
    fun onResponse(channel: Channel, baseMs: Long, isEmpty: Boolean): Long {
        val base = effectiveBase(baseMs)
        if (!isEmpty) {
            // Dolu yanıt: geri çekilmeyi sıfırla.
            gates[channel] = base
            return base
        }

        val ceiling = max(base, CEILING_MS)
        val next = min(((gates[channel] ?: base) * BACKOFF_MULTIPLIER).toLong(), ceiling)
        gates[channel] = next
        return next
    }

    /**
     * İsteği uçuşa al. Aynı kanalda uçuşta istek varsa `false` döner — çağıran isteği
     * göndermemelidir. Her `true` dönüşü bir [endRequest] ile eşlenmelidir.
     */
    @Synchronized
    fun beginRequest(channel: Channel): Boolean {
        val startedAt = inFlightSince[channel]
        val now = System.currentTimeMillis()
        if (startedAt != null && now - startedAt < IN_FLIGHT_TIMEOUT_MS) return false
        inFlightSince[channel] = now
        return true
    }

    /** İstek sonuçlandı (başarılı ya da hatalı): kanal yeniden uygun. */
    @Synchronized
    fun endRequest(channel: Channel) {
        inFlightSince.remove(channel)
    }

    /**
     * Uygulama ön plana geldi: geri çekilme sıfırlanır. Uçuş bayrakları korunur — o istekler hâlâ
     * yolda ve yanıtları geldiğinde bayrak zaten düşecek.
     */
    @Synchronized
    fun reset() {
        gates.clear()
    }

    private fun effectiveBase(baseMs: Long): Long = max(baseMs, MIN_BASE_MS)
}
