package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class AbTestAssignmentResponse(
    @SerializedName("contentId") val contentId: String?,
    @SerializedName("isControlGroup") val isControlGroup: Boolean = false
) : Serializable {

    companion object {
        const val CONTROL_GROUP_ID = "00000000-0000-0000-0000-000000000000"
    }

    fun isControlBucket(): Boolean {
        return isControlGroup || CONTROL_GROUP_ID.equals(contentId, ignoreCase = true)
    }
}
