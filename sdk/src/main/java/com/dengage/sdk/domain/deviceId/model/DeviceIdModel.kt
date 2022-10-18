package com.dengage.sdk.domain.deviceId.model


import com.google.gson.annotations.SerializedName
import java.io.Serializable

class DeviceIdModel (
    @SerializedName("device_id") val device_id: String,
) : Serializable