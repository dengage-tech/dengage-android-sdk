package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class AbTest(
    @SerializedName("variants") val variants: List<AbTestVariant>?
) : Serializable {

    /**
     * Tek varyantın yüzde 100 olması durumu: kazanan faz veya tek-bucket 100 konfigürasyonu.
     * Bu durumda /ab/assign çağrısına gerek yoktur, sonuç deterministtir.
     */
    fun getDeterministicVariant(): AbTestVariant? {
        val list = variants ?: return null
        if (list.size != 1) return null
        val only = list.first()
        val pct = only.percentage ?: return null
        return if (pct >= 100.0) only else null
    }

    fun findVariantByContentId(contentId: String?): AbTestVariant? {
        if (contentId.isNullOrEmpty()) return null
        return variants?.firstOrNull { it.contentId == contentId }
    }
}
