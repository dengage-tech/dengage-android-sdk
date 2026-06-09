package com.dengage.geofence.engine.storage.sqlite

import android.content.ContentValues
import android.database.sqlite.SQLiteDatabase
import com.dengage.geofence.engine.storage.EventQueueRepository
import com.dengage.geofence.engine.storage.model.QueuedEvent
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventType
import com.dengage.geofence.engine.storage.sqlite.GeofenceDbHelper.Companion.TABLE_EVENT_QUEUE

internal class SqliteEventQueueRepository(private val dbHelper: GeofenceDbHelper) :
    EventQueueRepository {

    override fun enqueue(event: QueuedEvent, maxSize: Int) {
        val db = dbHelper.writableDatabase
        val cv = ContentValues().apply {
            put("idempotency_key", event.idempotencyKey)
            put("geofence_id", event.geofenceId)
            put("cluster_id", event.clusterId)
            put("campaign_id", event.campaignId)
            put("event_type", event.eventType.wireValue)
            put("latitude", event.latitude)
            put("longitude", event.longitude)
            put("occurred_at", event.occurredAtMillis)
            put("created_at", System.currentTimeMillis())
        }
        // idempotency_key PK -> aynı trigger tekrar kuyruğa girmez (CONFLICT_IGNORE)
        db.insertWithOnConflict(TABLE_EVENT_QUEUE, null, cv, SQLiteDatabase.CONFLICT_IGNORE)
        trimToCap(db, maxSize)
    }

    override fun dequeueBatch(maxSize: Int): List<QueuedEvent> {
        val out = mutableListOf<QueuedEvent>()
        dbHelper.readableDatabase.query(
            TABLE_EVENT_QUEUE, null, null, null, null, null,
            "created_at ASC", maxSize.toString()
        ).use { c ->
            while (c.moveToNext()) {
                out.add(
                    QueuedEvent(
                        idempotencyKey = c.getString(c.getColumnIndexOrThrow("idempotency_key")),
                        geofenceId = c.getInt(c.getColumnIndexOrThrow("geofence_id")),
                        clusterId = c.getInt(c.getColumnIndexOrThrow("cluster_id")),
                        campaignId = c.getColumnIndexOrThrow("campaign_id").let { if (c.isNull(it)) null else c.getInt(it) },
                        eventType = wireToEventType(c.getString(c.getColumnIndexOrThrow("event_type"))),
                        latitude = c.getDouble(c.getColumnIndexOrThrow("latitude")),
                        longitude = c.getDouble(c.getColumnIndexOrThrow("longitude")),
                        occurredAtMillis = c.getLong(c.getColumnIndexOrThrow("occurred_at"))
                    )
                )
            }
        }
        return out
    }

    override fun ack(idempotencyKeys: List<String>) {
        if (idempotencyKeys.isEmpty()) return
        val db = dbHelper.writableDatabase
        db.beginTransaction()
        try {
            for (key in idempotencyKeys) {
                db.delete(TABLE_EVENT_QUEUE, "idempotency_key = ?", arrayOf(key))
            }
            db.setTransactionSuccessful()
        } finally {
            db.endTransaction()
        }
    }

    override fun size(): Int {
        dbHelper.readableDatabase.rawQuery("SELECT COUNT(*) FROM $TABLE_EVENT_QUEUE", null).use {
            return if (it.moveToFirst()) it.getInt(0) else 0
        }
    }

    override fun clear() {
        dbHelper.writableDatabase.delete(TABLE_EVENT_QUEUE, null, null)
    }

    /** Kuyruk cap'i aşarsa en eski kayıtları siler (offlineQueueMaxSize). */
    private fun trimToCap(db: SQLiteDatabase, maxSize: Int) {
        val count = size()
        if (count <= maxSize) return
        val toDelete = count - maxSize
        db.execSQL(
            """
            DELETE FROM $TABLE_EVENT_QUEUE WHERE idempotency_key IN (
                SELECT idempotency_key FROM $TABLE_EVENT_QUEUE ORDER BY created_at ASC LIMIT ?
            )
            """.trimIndent(),
            arrayOf(toDelete)
        )
    }

    private fun wireToEventType(value: String?): GeofenceEventType = when (value?.lowercase()) {
        "exit" -> GeofenceEventType.EXIT
        "dwell" -> GeofenceEventType.DWELL
        else -> GeofenceEventType.ENTER
    }
}
