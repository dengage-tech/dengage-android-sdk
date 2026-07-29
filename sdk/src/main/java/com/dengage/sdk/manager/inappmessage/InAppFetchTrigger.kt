package com.dengage.sdk.manager.inappmessage

/**
 * In-app fetch'ini tetikleyen olay. Fetch aralığının uygulanıp uygulanmayacağını belirler.
 */
enum class InAppFetchTrigger {

    /**
     * Uygulama kullanılabilir hale geldi: soğuk başlatma veya arka plandan ön plana dönüş.
     * Baskın varış yolu budur, o yüzden fetch aralığına takılmaz; yalnızca kazara çalkantıyı
     * eleyen küçük bir taban uygulanır.
     */
    APP_FOREGROUND,

    /** Periyodik tur, SDK ayarları yenilemesi veya uygulamanın manuel çağrısı. Aralık uygulanır. */
    OTHER,
}
