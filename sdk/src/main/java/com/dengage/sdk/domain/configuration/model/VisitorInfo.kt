package com.dengage.sdk.domain.configuration.model

import com.google.gson.annotations.SerializedName

data class VisitorInfo(
    @SerializedName("Segments") val segments: MutableList<Int>? = mutableListOf(),
    @SerializedName("Tags") val tags: MutableList<Int>? = mutableListOf()
)
