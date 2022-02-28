package com.dengage.sdk.domain.tag.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class TagsRequest(
    @SerializedName("accountName") val accountName: String,
    @SerializedName("key") val key: String,
    @SerializedName("tags") val tags: List<TagItem>
) : Serializable
