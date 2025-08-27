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
    @SerializedName("field") val field: String? = null,
    @SerializedName("event") val event: String? = null,
    @SerializedName("window") val window: String? = null,
    @SerializedName("filtersLogicalOp") val filtersLogicalOp: String? = null,
    @SerializedName("filters") val filters: List<Filter>? = null
) : Serializable

data class Filter(
    @SerializedName("field") val field: String,
    @SerializedName("op") val op: String,
    @SerializedName("values") val values: List<String>
) : Serializable