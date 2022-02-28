package com.dengage.sdk.domain.inappmessage.model

import org.junit.Assert
import org.junit.Test

class DisplayConditionTest {

    @Test
    fun `DisplayCondition constructor test`() {
        val screenName = "screenName"
        val operator = Operator.STARTS_WITH.operator
        val displayCondition = DisplayCondition(
            screenNameFilters = listOf(ScreenNameFilter(
                value = listOf(screenName),
                operator = operator
            )),
            screenDataFilters = null
        )
        Assert.assertEquals(displayCondition.screenNameFilters?.get(0)?.value?.get(0), screenName)
        Assert.assertEquals(displayCondition.screenNameFilters?.get(0)?.operator, operator)
    }

}