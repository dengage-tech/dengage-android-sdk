package com.dengage.sdk.domain.configuration.model

import org.junit.Assert
import org.junit.Test

class AppTrackingTest {

    @Test
    fun `AppTracking constructor test`() {
        val alias = "alias"
        val packageName = "packageName"

        val appTracking = AppTracking(
            alias = alias,
            packageName = packageName
        )

        Assert.assertEquals(alias, appTracking.alias)
        Assert.assertEquals(packageName, appTracking.packageName)
    }

}