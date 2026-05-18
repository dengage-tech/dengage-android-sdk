package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class AbTestVariant(
    @SerializedName("contentId") val contentId: String?,
    @SerializedName("percentage") val percentage: Double?,
    @SerializedName("isControlGroup") val isControlGroup: Boolean? = false,
    @SerializedName("type") val type: String? = null,
    @SerializedName("props") val params: ContentParams? = null
) : Serializable {

    fun isControl(): Boolean = isControlGroup == true

    fun toContent(): Content? {
        if (isControl() || type.isNullOrEmpty() || params == null) return null
        return Content(
            type = type,
            targetUrl = null,
            params = params,
            contentId = contentId
        )
    }
}
