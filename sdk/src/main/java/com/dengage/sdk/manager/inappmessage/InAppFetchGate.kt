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
 * - Taban: hesabın kanal bazlı fetch aralığı ayarı (sunucudan gelir, hesap bazında değiştirilebilir)
 * - Tavan: `max(taban, 15 dk)` — ayarını tavanın üstüne çekmiş bir hesapta geri çekilme, hesabın
 *   istediğinden daha sık fetch etmeye **dönüşmemeli**
 * - Sıfırlama: uygulama ön plana geldiğinde. Oturum başı çapası kayan pencereyle birlikte
 *   neredeyse hiç tetiklenmeyen bir olaya dönüştüğü için tercih edilmedi.
 *
 * Durum yalnızca bellekte tutulur: process ölürse zaten sıfırlanması gerekir.
 */
internal object InAppFetchGate {

    enum class Channel { BULK, REAL_TIME }

    private const val BACKOFF_MULTIPLIER = 1.2
    private const val CEILING_MS = 15 * 60 * 1000L

    private val gates = mutableMapOf<Channel, Long>()

    /** Kanalın yürürlükteki gate'i (ms). */
    @Synchronized
    fun current(channel: Channel, baseMs: Long): Long = gates[channel] ?: baseMs

    /** Yanıt sonrası gate'i güncelle ve yeni değeri döndür. */
    @Synchronized
    fun onResponse(channel: Channel, baseMs: Long, isEmpty: Boolean): Long {
        if (!isEmpty) {
            // Dolu yanıt: geri çekilmeyi sıfırla.
            gates[channel] = baseMs
            return baseMs
        }

        val ceiling = max(baseMs, CEILING_MS)
        val next = min(((gates[channel] ?: baseMs) * BACKOFF_MULTIPLIER).toLong(), ceiling)
        gates[channel] = next
        return next
    }

    /** Uygulama ön plana geldi: geri çekilme sıfırlanır. */
    @Synchronized
    fun reset() {
        gates.clear()
    }
}
