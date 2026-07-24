package com.dengage.geofenceengine.storage

import com.dengage.geofenceengine.storage.model.Fence

/** Fence persist + spatial query (doc 21 §6.2). Default impl SQLite + R*Tree. */
interface FenceRepository {
    fun save(fences: List<Fence>)
    fun loadAll(): List<Fence>

    /**
     * Cihaz konumuna en yakın [limit] fence (haversine sort).
     * [activeOnly] = true ise sadece `activeNow` fence'ler döner (K7 gate).
     */
    fun nearest(lat: Double, lon: Double, limit: Int, activeOnly: Boolean): List<Fence>
    fun findById(geofenceId: Int): Fence?
    fun clear()
}
