package com.dengage.sdk.domain.configuration.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class EventTypeDefinition(
    @SerializedName("eventTypeId") val eventTypeId: Int?,
    @SerializedName("eventType") val eventType: String?,
    @SerializedName("logicOperator") val logicOperator: String?,
    @SerializedName("filterConditions") val filterConditions: List<FilterCondition>?
) : Serializable