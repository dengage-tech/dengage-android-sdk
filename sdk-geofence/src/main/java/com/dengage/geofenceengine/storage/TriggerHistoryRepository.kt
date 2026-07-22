package com.dengage.geofenceengine.storage

import com.dengage.geofenceengine.storage.model.TriggerHistoryEntry

/**
 * Tetiklenen transition'ların kısa geçmişi (teşhis/QA). Event kuyruğu yalnızca *gönderilmemiş*
 * event'leri tutar (başarılıysa ack'lenip silinir), dolayısıyla "ne tetiklendi?" sorusunu
 * cevaplayamaz. Bu depo sabit boyutludur; en yeni kayıt başta döner.
 */
interface TriggerHistoryRepository {
    /** [maxSize] aşılırsa en eski kayıtlar düşürülür. */
    fun record(entry: TriggerHistoryEntry, maxSize: Int)

    /** En yeniden eskiye sıralı. */
    fun recent(limit: Int): List<TriggerHistoryEntry>

    fun clear()
}
