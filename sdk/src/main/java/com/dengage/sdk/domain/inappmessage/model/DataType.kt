package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName

enum class DataType {
    @SerializedName("INT")
    INT,

    @SerializedName("TEXT")
    TEXT,

    @SerializedName("BOOL")
    BOOL,

    @SerializedName("TEXTLIST")
    TEXT_LIST,

    @SerializedName("DATETIME")
    DATETIME,

    @SerializedName("VISITCOUNTPASTXDAYS")
    VISIT_COUNT_PAST_X_DAYS,
}