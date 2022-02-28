package com.dengage.sdk.domain.inappmessage.model

import org.junit.Assert
import org.junit.Test

class DisplayTimingTest {

    @Test
    fun `DisplayTiming constructor test`() {
        val triggerBy = TriggerBy.NAVIGATION.triggerBy
        val delay = 10
        val showEveryXMinutes = 20

        val displayTiming = DisplayTiming(
            triggerBy = triggerBy,
            delay = delay,
            showEveryXMinutes = showEveryXMinutes
        )

        Assert.assertEquals(triggerBy, displayTiming.triggerBy)
        Assert.assertEquals(delay, displayTiming.delay)
        Assert.assertEquals(showEveryXMinutes, displayTiming.showEveryXMinutes)
    }

}