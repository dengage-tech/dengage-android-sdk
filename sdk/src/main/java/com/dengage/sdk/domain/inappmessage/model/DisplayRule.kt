package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class DisplayRule(
    @SerializedName("logicOperatorBetweenCriterions") val logicOperator: String,
    @SerializedName("criterions") val criterionList: List<Criterion>
) : Serializable
