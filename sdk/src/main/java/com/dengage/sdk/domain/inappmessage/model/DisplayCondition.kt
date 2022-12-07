package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

class DisplayCondition(
    @SerializedName("screenNameFilters") val screenNameFilters: List<ScreenNameFilter>?,
    @SerializedName("ruleSet") val displayRuleSet: DisplayRuleSet? = null
) : Serializable
