package com.dengage.geofence.engine.storage

import com.dengage.geofence.engine.storage.model.QueuedEvent

/** Offline event kuyruğu (doc 21 §6.2). Online olunca batch flush edilir. */
interface EventQueueRepository {
    /** [maxSize] aşılırsa en eski event düşürülür (offlineQueueMaxSize cap). */
    fun enqueue(event: QueuedEvent, maxSize: Int)
    fun dequeueBatch(maxSize: Int): List<QueuedEvent>
    fun ack(idempotencyKeys: List<String>)
    fun size(): Int
    fun clear()
}
