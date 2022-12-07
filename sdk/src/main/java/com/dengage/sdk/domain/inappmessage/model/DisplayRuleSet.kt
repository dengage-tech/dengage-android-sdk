package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class DisplayRuleSet(
    @SerializedName("logicOperator") val logicOperator: String,
    @SerializedName("rules") val displayRules: List<DisplayRule>
) : Serializable
