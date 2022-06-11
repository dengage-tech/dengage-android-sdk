package com.dengage.sdk.domain.geofence.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable
import java.util.*

class GeofenceEvent(
    @SerializedName("identifier") val identifier: String,
    @SerializedName("cid") val cid: Int,
    @SerializedName("geoid") val geoid: Int,
    @SerializedName("type") val type: String,
    @SerializedName("et") var et: Long,
    @SerializedName("pp") var pp: Boolean
) : Serializable
