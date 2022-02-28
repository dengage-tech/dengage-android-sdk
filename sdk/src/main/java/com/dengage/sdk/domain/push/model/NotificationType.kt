package com.dengage.sdk.domain.push.model

import com.google.gson.annotations.SerializedName

enum class NotificationType(val type: String) {
    @SerializedName("TEXT")
    TEXT("TEXT"),

    @SerializedName("RICH")
    RICH("RICH"),

    @SerializedName("CAROUSEL")
    CAROUSEL("CAROUSEL")
}