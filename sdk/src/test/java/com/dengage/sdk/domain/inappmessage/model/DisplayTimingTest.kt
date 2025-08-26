package com.dengage.sdk.domain.inappmessage.model

import org.junit.Assert
import org.junit.Test

class DisplayTimingTest {

    @Test
    fun `DisplayTiming constructor test`() {
        val delay = 10
        val showEveryXMinutes = 20
        val maxShowCount = 20
        val maxDismissCount = 5

        val displayTiming = DisplayTiming(
            delay = delay,
            showEveryXMinutes = showEveryXMinutes,
            maxShowCount = maxShowCount,
            maxDismissCount = maxDismissCount
        )

        Assert.assertEquals(delay, displayTiming.delay)
        Assert.assertEquals(showEveryXMinutes, displayTiming.showEveryXMinutes)
        Assert.assertEquals(maxShowCount, displayTiming.maxShowCount)
        Assert.assertEquals(maxDismissCount, displayTiming.maxDismissCount)
    }

}