package com.dengage.geofenceengine.storage

import android.content.Context
import com.dengage.geofenceengine.storage.sqlite.GeofenceDbHelper
import com.dengage.geofenceengine.storage.sqlite.SqliteDeviceStateRepository
import com.dengage.geofenceengine.storage.sqlite.SqliteEventQueueRepository
import com.dengage.geofenceengine.storage.sqlite.SqliteFenceRepository

/**
 * Engine storage erişim noktası. Tek bir [GeofenceDbHelper] üzerinden tüm repo'ları sağlar.
 * Default impl SQLite + R*Tree (doc 21 §6.3). İleride in-memory/JSON alt impl buradan değiştirilebilir.
 */
class GeofenceStorage(context: Context) {

    private val dbHelper = GeofenceDbHelper(context.applicationContext)

    val fenceRepository: FenceRepository = SqliteFenceRepository(dbHelper)
    val deviceStateRepository: DeviceStateRepository = SqliteDeviceStateRepository(dbHelper)
    val eventQueueRepository: EventQueueRepository = SqliteEventQueueRepository(dbHelper)
    val syncMetadataRepository: SyncMetadataRepository = PrefsSyncMetadataRepository()

    fun clearAll() {
        fenceRepository.clear()
        deviceStateRepository.clear()
        eventQueueRepository.clear()
    }
}
