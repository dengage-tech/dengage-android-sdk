package com.dengage.sdk.domain.inappmessage.model

enum class Operator(val operator: String) {
    EQUALS("EQUALS"), // eşittir
    NOT_EQUALS("NOT_EQUALS"), // eşit değildir
    LIKE("LIKE"), // ScreenName içerisinde geçiyorsa. Mesela value 'cart' ise, o zaman screen name içerisinde cart geçiyorsa
    NOT_LIKE("NOT_LIKE"), // ScreenName içinde geçmiyorsa
    STARTS_WITH("STARTS_WITH"), // ScreenName value'daki string ile başlıyorsa
    NOT_STARTS_WITH("NOT_STARTS_WITH"), // ScreenName value'daki string ile başlamıyorsa
    ENDS_WITH("ENDS_WITH"), // ScreenName value'daki string ile bitiyorsa
    NOT_ENDS_WITH("NOT_ENDS_WITH"), // ScreenName value'daki string ile bitmiyorsa
    IN("IN"), // ScreenName value içerisindeki değerlerden biriyse. value 'cart|product' gibi | ile ayrılmış değerlerdir
    NOT_IN("NOT_IN") // ScreenName value içerisindeki değerlerden biri değilse. value 'cart|product' gibi | ile ayrılmış değerlerdir    eşittir
}
