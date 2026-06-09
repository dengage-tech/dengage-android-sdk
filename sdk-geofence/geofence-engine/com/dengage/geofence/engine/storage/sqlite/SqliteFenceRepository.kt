package com.dengage.geofence.engine.storage.sqlite

import android.content.ContentValues
import com.dengage.geofence.engine.storage.FenceRepository
import com.dengage.geofence.engine.storage.model.Fence
import com.dengage.geofence.engine.storage.sqlite.GeofenceDbHelper.Companion.TABLE_FENCES
import com.dengage.geofence.engine.storage.sqlite.GeofenceDbHelper.Companion.TABLE_FENCES_RTREE
import kotlin.math.asin
import kotlin.math.cos
import kotlin.math.pow
import kotlin.math.sin
import kotlin.math.sqrt

/**
 * Default [FenceRepository] impl: SQLite + R*Tree (doc 21 §6.3).
 * `nearest` haversine sort kullanır; R*Tree spatial index `save`'de korunur (audience/ileri kullanım).
 */
internal class SqliteFenceRepository(private val dbHelper: GeofenceDbHelper) : FenceRepository {

    private val hasRTree: Boolean by lazy { dbHelper.hasRTree() }

    override fun save(fences: List<Fence>) {
        val db = dbHelper.writableDatabase
        db.beginTransaction()
        try {
            db.delete(TABLE_FENCES, null, null)
            if (hasRTree) runCatching { db.delete(TABLE_FENCES_RTREE, null, null) }
            for (f in fences) {
                val cv = ContentValues().apply {
                    put("fence_id", f.fenceId)
                    put("cluster_id", f.clusterId)
                    put("latitude", f.latitude)
                    put("longitude", f.longitude)
                    put("radius_m", f.radiusM)
                    put("title", f.title)
                    put("active_now", if (f.activeNow) 1 else 0)
                    put("next_state_change_at", f.nextStateChangeAtMillis)
                    put("next_state_change_to", f.nextStateChangeTo)
                    put("campaigns_json", EngineGson.campaignsToJson(f.campaigns))
                }
                db.insertWithOnConflict(TABLE_FENCES, null, cv, android.database.sqlite.SQLiteDatabase.CONFLICT_REPLACE)

                if (hasRTree) {
                    runCatching {
                        val rcv = ContentValues().apply {
                            put("id", f.fenceId)
                            put("min_lat", f.latitude)
                            put("max_lat", f.latitude)
                            put("min_lon", f.longitude)
                            put("max_lon", f.longitude)
                        }
                        db.insertWithOnConflict(TABLE_FENCES_RTREE, null, rcv, android.database.sqlite.SQLiteDatabase.CONFLICT_REPLACE)
                    }
                }
            }
            db.setTransactionSuccessful()
        } finally {
            db.endTransaction()
        }
    }

    override fun loadAll(): List<Fence> = query(null, null)

    override fun nearest(lat: Double, lon: Double, limit: Int, activeOnly: Boolean): List<Fence> {
        val selection = if (activeOnly) "active_now = 1" else null
        return query(selection, null)
            .sortedBy { haversineKm(lat, lon, it.latitude, it.longitude) }
            .take(limit)
    }

    override fun findById(fenceId: Int): Fence? =
        query("fence_id = ?", arrayOf(fenceId.toString())).firstOrNull()

    override fun clear() {
        val db = dbHelper.writableDatabase
        db.delete(TABLE_FENCES, null, null)
        if (hasRTree) runCatching { db.delete(TABLE_FENCES_RTREE, null, null) }
    }

    private fun query(selection: String?, args: Array<String>?): List<Fence> {
        val out = mutableListOf<Fence>()
        dbHelper.readableDatabase.query(
            TABLE_FENCES, null, selection, args, null, null, null
        ).use { c ->
            val iId = c.getColumnIndexOrThrow("fence_id")
            val iCluster = c.getColumnIndexOrThrow("cluster_id")
            val iLat = c.getColumnIndexOrThrow("latitude")
            val iLon = c.getColumnIndexOrThrow("longitude")
            val iRadius = c.getColumnIndexOrThrow("radius_m")
            val iTitle = c.getColumnIndexOrThrow("title")
            val iActive = c.getColumnIndexOrThrow("active_now")
            val iNext = c.getColumnIndexOrThrow("next_state_change_at")
            val iNextTo = c.getColumnIndexOrThrow("next_state_change_to")
            val iCampaigns = c.getColumnIndexOrThrow("campaigns_json")
            while (c.moveToNext()) {
                out.add(
                    Fence(
                        fenceId = c.getInt(iId),
                        clusterId = c.getInt(iCluster),
                        latitude = c.getDouble(iLat),
                        longitude = c.getDouble(iLon),
                        radiusM = c.getDouble(iRadius),
                        title = if (c.isNull(iTitle)) null else c.getString(iTitle),
                        activeNow = c.getInt(iActive) == 1,
                        nextStateChangeAtMillis = if (c.isNull(iNext)) null else c.getLong(iNext),
                        nextStateChangeTo = if (c.isNull(iNextTo)) null else c.getString(iNextTo),
                        campaigns = EngineGson.campaignsFromJson(
                            if (c.isNull(iCampaigns)) null else c.getString(iCampaigns)
                        )
                    )
                )
            }
        }
        return out
    }

    private fun haversineKm(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double {
        val r = 6372.8
        val dLat = Math.toRadians(lat2 - lat1)
        val dLon = Math.toRadians(lon2 - lon1)
        val a = sin(dLat / 2).pow(2.0) +
            sin(dLon / 2).pow(2.0) * cos(Math.toRadians(lat1)) * cos(Math.toRadians(lat2))
        return r * 2 * asin(sqrt(a))
    }
}
