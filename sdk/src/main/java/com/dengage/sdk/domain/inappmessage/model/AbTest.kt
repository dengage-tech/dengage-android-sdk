package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class AbTest(
    @SerializedName("variants") val variants: MutableList<AbTestVariant>?
) : Serializable {

    fun isDeterministic(): Boolean {
        val list = variants ?: return false
        return list.size == 1 && list[0].percentage >= 100.0
    }
}
