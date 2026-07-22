package com.dengage.geofenceengine.storage.sqlite

import android.content.ContentValues
import com.dengage.geofenceengine.storage.TriggerHistoryRepository
import com.dengage.geofenceengine.storage.model.TriggerHistoryEntry
import com.dengage.geofenceengine.storage.sqlite.GeofenceDbHelper.Companion.TABLE_TRIGGER_HISTORY
import com.dengage.sdk.domain.geofence.model.sync.GeofenceEventType

internal class SqliteTriggerHistoryRepository(private val dbHelper: GeofenceDbHelper) :
    TriggerHistoryRepository {

    override fun record(entry: TriggerHistoryEntry, maxSize: Int) {
        val db = dbHelper.writableDatabase
        val cv = ContentValues().apply {
            put("geofence_id", entry.geofenceId)
            put("cluster_id", entry.clusterId)
            put("title", entry.title)
            put("event_type", entry.eventType.wireValue)
            put("occurred_at", entry.occurredAtMillis)
            put("campaign_ids", entry.campaignIds.joinToString(","))
            put("accuracy_m", entry.accuracyM)
            put("state_only", if (entry.stateOnly) 1 else 0)
        }
        db.insert(TABLE_TRIGGER_HISTORY, null, cv)

        // Cap: en yeni [maxSize] kayıt kalsın, eskiler düşsün.
        db.execSQL(
            """
            DELETE FROM $TABLE_TRIGGER_HISTORY WHERE id NOT IN (
                SELECT id FROM $TABLE_TRIGGER_HISTORY ORDER BY id DESC LIMIT ?
            )
            """.trimIndent(),
            arrayOf(maxSize.toString())
        )
    }

    override fun recent(limit: Int): List<TriggerHistoryEntry> {
        val out = mutableListOf<TriggerHistoryEntry>()
        dbHelper.readableDatabase.query(
            TABLE_TRIGGER_HISTORY, null, null, null, null, null,
            "id DESC", limit.toString()
        ).use { c ->
            while (c.moveToNext()) {
                val titleIndex = c.getColumnIndexOrThrow("title")
                val campaignIds = c.getString(c.getColumnIndexOrThrow("campaign_ids"))
                    ?.split(",")
                    ?.mapNotNull { it.trim().toIntOrNull() }
                    ?: emptyList()
                out.add(
                    TriggerHistoryEntry(
                        geofenceId = c.getInt(c.getColumnIndexOrThrow("geofence_id")),
                        clusterId = c.getInt(c.getColumnIndexOrThrow("cluster_id")),
                        title = if (c.isNull(titleIndex)) null else c.getString(titleIndex),
                        eventType = wireToEventType(c.getString(c.getColumnIndexOrThrow("event_type"))),
                        occurredAtMillis = c.getLong(c.getColumnIndexOrThrow("occurred_at")),
                        campaignIds = campaignIds,
                        accuracyM = c.getColumnIndexOrThrow("accuracy_m").let { if (c.isNull(it)) null else c.getDouble(it) },
                        stateOnly = c.getInt(c.getColumnIndexOrThrow("state_only")) == 1
                    )
                )
            }
        }
        return out
    }

    override fun clear() {
        dbHelper.writableDatabase.delete(TABLE_TRIGGER_HISTORY, null, null)
    }

    private fun wireToEventType(value: String?): GeofenceEventType = when (value?.lowercase()) {
        "exit" -> GeofenceEventType.EXIT
        "dwell" -> GeofenceEventType.DWELL
        else -> GeofenceEventType.ENTER
    }
}
