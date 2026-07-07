package com.dengage.geofenceengine.storage.sqlite

import android.content.ContentValues
import android.database.sqlite.SQLiteDatabase
import com.dengage.geofenceengine.storage.DeviceStateRepository
import com.dengage.geofenceengine.storage.model.DeviceFenceState
import com.dengage.geofenceengine.storage.model.FenceState
import com.dengage.geofenceengine.storage.sqlite.GeofenceDbHelper.Companion.TABLE_DEVICE_STATE

internal class SqliteDeviceStateRepository(private val dbHelper: GeofenceDbHelper) :
    DeviceStateRepository {

    override fun setState(
        geofenceId: Int,
        clusterId: Int,
        state: FenceState,
        enteredAt: Long?,
        lastSeenAt: Long?,
        exitedAt: Long?
    ) {
        val cv = ContentValues().apply {
            put("fence_id", geofenceId)
            put("cluster_id", clusterId)
            put("state", state.wireValue)
            put("entered_at", enteredAt)
            put("last_seen_at", lastSeenAt)
            put("exited_at", exitedAt)
        }
        dbHelper.writableDatabase.insertWithOnConflict(
            TABLE_DEVICE_STATE, null, cv, SQLiteDatabase.CONFLICT_REPLACE
        )
    }

    override fun getState(geofenceId: Int): DeviceFenceState? {
        dbHelper.readableDatabase.query(
            TABLE_DEVICE_STATE, null, "fence_id = ?", arrayOf(geofenceId.toString()),
            null, null, null
        ).use { c ->
            if (!c.moveToFirst()) return null
            return DeviceFenceState(
                geofenceId = c.getInt(c.getColumnIndexOrThrow("fence_id")),
                clusterId = c.getInt(c.getColumnIndexOrThrow("cluster_id")),
                state = FenceState.fromWire(c.getString(c.getColumnIndexOrThrow("state"))),
                enteredAt = c.getColumnIndexOrThrow("entered_at").let { if (c.isNull(it)) null else c.getLong(it) },
                lastSeenAt = c.getColumnIndexOrThrow("last_seen_at").let { if (c.isNull(it)) null else c.getLong(it) },
                exitedAt = c.getColumnIndexOrThrow("exited_at").let { if (c.isNull(it)) null else c.getLong(it) }
            )
        }
    }

    override fun clear() {
        dbHelper.writableDatabase.delete(TABLE_DEVICE_STATE, null, null)
    }
}
