package com.dengage.sdk.domain.inappmessage.model

import org.junit.Assert
import org.junit.Test

class ScreenDataFilterTest {

    @Test
    fun `ScreenDataFilter constructor test`() {
        val dataName = "dataName"
        val type = "type"
        val screenName = "screenName"
        val operator = Operator.STARTS_WITH.operator
        val screenDataFilter = ScreenDataFilter(
            dataName = dataName,
            type = type,
            value = listOf(screenName),
            operator = operator
        )

        Assert.assertEquals(screenDataFilter.dataName, dataName)
        Assert.assertEquals(screenDataFilter.type, type)
        Assert.assertEquals(screenDataFilter.value[0], screenName)
        Assert.assertEquals(screenDataFilter.operator, operator)
    }

}