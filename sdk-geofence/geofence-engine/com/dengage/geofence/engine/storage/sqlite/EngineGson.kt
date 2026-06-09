package com.dengage.geofence.engine.storage.sqlite

import com.dengage.sdk.domain.geofence.model.sync.SyncCampaign
import com.google.gson.Gson
import com.google.gson.reflect.TypeToken

/** Engine storage için paylaşılan Gson örneği (campaigns JSON blob serialize/deserialize). */
internal object EngineGson {
    val gson: Gson = Gson()

    private val campaignListType = object : TypeToken<List<SyncCampaign>>() {}.type

    fun campaignsToJson(campaigns: List<SyncCampaign>): String =
        gson.toJson(campaigns, campaignListType)

    fun campaignsFromJson(json: String?): List<SyncCampaign> {
        if (json.isNullOrBlank()) return emptyList()
        return runCatching {
            gson.fromJson<List<SyncCampaign>>(json, campaignListType) ?: emptyList()
        }.getOrDefault(emptyList())
    }
}
