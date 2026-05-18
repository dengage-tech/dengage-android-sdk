package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class AbTestAssignment(
    @SerializedName("contentId") val contentId: String?,
    @SerializedName("isControlGroup") val isControlGroup: Boolean? = false
) : Serializable {
    fun isControl(): Boolean = isControlGroup == true
}
