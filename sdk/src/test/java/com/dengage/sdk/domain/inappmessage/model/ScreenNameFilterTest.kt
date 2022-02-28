package com.dengage.sdk.domain.inappmessage.model

import org.junit.Assert
import org.junit.Test

class ScreenNameFilterTest {

    @Test
    fun `ScreenNameFilter constructor test`() {
        val screenName = "screenName"
        val operator = Operator.STARTS_WITH.operator
        val screenNameFilter = ScreenNameFilter(
            value = listOf(screenName),
            operator = operator
        )

        Assert.assertEquals(screenNameFilter.value[0], screenName)
        Assert.assertEquals(screenNameFilter.operator, operator)
    }

}