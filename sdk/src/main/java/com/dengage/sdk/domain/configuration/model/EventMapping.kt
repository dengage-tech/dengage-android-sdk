package com.dengage.sdk.domain.configuration.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class EventMapping(
    @SerializedName("eventTableName") val eventTableName: String?,
    @SerializedName("eventTypeDefinitions") val eventTypeDefinitions: List<EventTypeDefinition>?
) : Serializable