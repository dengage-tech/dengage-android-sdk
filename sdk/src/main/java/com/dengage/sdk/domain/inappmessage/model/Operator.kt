package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName

enum class Operator(val operator: String) {
    @SerializedName("EQUALS")
    EQUALS("EQUALS"), // eşittir

    @SerializedName("NOT_EQUALS")
    NOT_EQUALS("NOT_EQUALS"), // eşit değildir

    @SerializedName("LIKE")
    LIKE("LIKE"), // ScreenName içerisinde geçiyorsa. Mesela value 'cart' ise, o zaman screen name içerisinde cart geçiyorsa

    @SerializedName("NOT_LIKE")
    NOT_LIKE("NOT_LIKE"), // ScreenName içinde geçmiyorsa

    @SerializedName("STARTS_WITH")
    STARTS_WITH("STARTS_WITH"), // ScreenName value'daki string ile başlıyorsa

    @SerializedName("NOT_STARTS_WITH")
    NOT_STARTS_WITH("NOT_STARTS_WITH"), // ScreenName value'daki string ile başlamıyorsa

    @SerializedName("ENDS_WITH")
    ENDS_WITH("ENDS_WITH"), // ScreenName value'daki string ile bitiyorsa

    @SerializedName("NOT_ENDS_WITH")
    NOT_ENDS_WITH("NOT_ENDS_WITH"), // ScreenName value'daki string ile bitmiyorsa

    @SerializedName("IN")
    IN("IN"), // ScreenName value içerisindeki değerlerden biriyse. value 'cart|product' gibi | ile ayrılmış değerlerdir

    @SerializedName("NOT_IN")
    NOT_IN("NOT_IN"), // ScreenName value içerisindeki değerlerden biri değilse. value 'cart|product' gibi | ile ayrılmış değerlerdir    eşittir

    @SerializedName("GREATER_THAN")
    GREATER_THAN("GREATER_THAN"),

    @SerializedName("GREATER_EQUAL")
    GREATER_EQUAL("GREATER_EQUAL"),

    @SerializedName("LESS_THAN")
    LESS_THAN("LESS_THAN"),

    @SerializedName("LESS_EQUAL")
    LESS_EQUAL("LESS_EQUAL"),

    @SerializedName("BETWEEN")
    BETWEEN("BETWEEN"),

    @SerializedName("NOT_BETWEEN")
    NOT_BETWEEN("NOT_BETWEEN"),

    @SerializedName("CONTAINS_ALL")
    CONTAINS_ALL("CONTAINS_ALL"),

    @SerializedName("CONTAINS_ANY")
    CONTAINS_ANY("CONTAINS_ANY")
}
