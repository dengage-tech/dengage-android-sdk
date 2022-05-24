package com.dengage.sdk.domain.event.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class LogoutEvent(
    @SerializedName("accountId") var accountId: Int?,
    @SerializedName("integrationKey") var integrationKey: String,
    @SerializedName("key") var key: String?,
    @SerializedName("eventTable") var eventTableName: String,
    @SerializedName("eventDetails") var eventDetails: MutableMap<String, Any>
) : Serializable