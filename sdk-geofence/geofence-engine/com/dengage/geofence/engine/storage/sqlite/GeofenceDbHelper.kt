package com.dengage.geofence.engine.storage.sqlite

import android.content.Context
import android.database.sqlite.SQLiteDatabase
import android.database.sqlite.SQLiteOpenHelper

/**
 * Geofence Engine SQLite store (doc 21 §6.3). R*Tree virtual table ile fence bounding-box prefilter.
 *
 * Tablolar:
 *  - fences          : fence metadata (campaigns JSON blob)
 *  - fences_rtree    : R*Tree spatial index (fence_id, min/max lat/lon)
 *  - device_state    : cihaz başına fence state (dwell tracking)
 *  - event_queue     : offline event kuyruğu
 */
internal class GeofenceDbHelper(context: Context) :
    SQLiteOpenHelper(context.applicationContext, DB_NAME, null, DB_VERSION) {

    override fun onCreate(db: SQLiteDatabase) {
        db.execSQL(
            """
            CREATE TABLE $TABLE_FENCES (
                fence_id INTEGER PRIMARY KEY,
                cluster_id INTEGER NOT NULL,
                latitude REAL NOT NULL,
                longitude REAL NOT NULL,
                radius_m REAL NOT NULL,
                title TEXT,
                active_now INTEGER NOT NULL DEFAULT 0,
                next_state_change_at INTEGER,
                next_state_change_to TEXT,
                campaigns_json TEXT
            )
            """.trimIndent()
        )

        // R*Tree: bounding box prefilter. id = fence_id.
        runCatching {
            db.execSQL(
                """
                CREATE VIRTUAL TABLE $TABLE_FENCES_RTREE USING rtree(
                    id, min_lat, max_lat, min_lon, max_lon
                )
                """.trimIndent()
            )
        }.onFailure {
            // R*Tree modülü bazı OEM SQLite build'lerinde yok. Haversine-only fallback ile devam.
        }

        db.execSQL(
            """
            CREATE TABLE $TABLE_DEVICE_STATE (
                fence_id INTEGER PRIMARY KEY,
                cluster_id INTEGER NOT NULL,
                state TEXT NOT NULL,
                entered_at INTEGER,
                last_seen_at INTEGER,
                exited_at INTEGER
            )
            """.trimIndent()
        )

        db.execSQL(
            """
            CREATE TABLE $TABLE_EVENT_QUEUE (
                idempotency_key TEXT PRIMARY KEY,
                geofence_id INTEGER NOT NULL,
                cluster_id INTEGER NOT NULL,
                campaign_id INTEGER,
                event_type TEXT NOT NULL,
                latitude REAL NOT NULL,
                longitude REAL NOT NULL,
                occurred_at INTEGER NOT NULL,
                created_at INTEGER NOT NULL
            )
            """.trimIndent()
        )
    }

    override fun onUpgrade(db: SQLiteDatabase, oldVersion: Int, newVersion: Int) {
        db.execSQL("DROP TABLE IF EXISTS $TABLE_FENCES")
        runCatching { db.execSQL("DROP TABLE IF EXISTS $TABLE_FENCES_RTREE") }
        db.execSQL("DROP TABLE IF EXISTS $TABLE_DEVICE_STATE")
        db.execSQL("DROP TABLE IF EXISTS $TABLE_EVENT_QUEUE")
        onCreate(db)
    }

    /** R*Tree virtual table'ın gerçekten oluşup oluşmadığını kontrol eder. */
    fun hasRTree(): Boolean = runCatching {
        readableDatabase.rawQuery(
            "SELECT name FROM sqlite_master WHERE type='table' AND name=?",
            arrayOf(TABLE_FENCES_RTREE)
        ).use { it.moveToFirst() }
    }.getOrDefault(false)

    companion object {
        const val DB_NAME = "dengage_geofence_engine.db"
        const val DB_VERSION = 1

        const val TABLE_FENCES = "fences"
        const val TABLE_FENCES_RTREE = "fences_rtree"
        const val TABLE_DEVICE_STATE = "device_state"
        const val TABLE_EVENT_QUEUE = "event_queue"
    }
}
