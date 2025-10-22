package com.dengage.sdk.domain.configuration.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class FilterCondition(
    @SerializedName("fieldName") val fieldName: String?,
    @SerializedName("operator") val operator: String?,
    @SerializedName("values") val values: List<String>?
) : Serializable