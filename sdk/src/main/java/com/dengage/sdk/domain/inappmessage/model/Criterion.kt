package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class Criterion(
    @SerializedName("id") val id: Long,
    @SerializedName("parameter") val parameter: String,
    @SerializedName("dataType") val dataType: String,
    @SerializedName("comparison") val operator: String,
    @SerializedName("values") val values: List<String>?,
    @SerializedName("valueSource") val valueSource: String,
    // New optional properties for EVENT_HISTORY
    @SerializedName("aggregateType") val aggregateType: String? = null,
    @SerializedName("aggregateField") val aggregateField: String? = null,
    @SerializedName("eventType") val eventType: String? = null,
    @SerializedName("timeWindow") val timeWindow: TimeWindow? = null,
    @SerializedName("filtersLogicalOp") val filtersLogicalOp: String? = null,
    @SerializedName("filters") val filters: List<EventFilter>? = null
) : Serializable

data class TimeWindow(
    @SerializedName("type") val type: String,
    @SerializedName("unit") val unit: String,
    @SerializedName("value") val value: String
) : Serializable

data class EventFilter(
    @SerializedName("parameter") val parameter: String,
    @SerializedName("comparison") val comparison: String,
    @SerializedName("dataType") val dataType: String,
    @SerializedName("values") val values: List<String>
) : Serializable