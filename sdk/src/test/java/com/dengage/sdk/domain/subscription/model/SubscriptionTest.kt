package com.dengage.sdk.domain.subscription.model

import org.junit.Assert
import org.junit.Test

class SubscriptionTest {

    @Test
    fun `Subscription constructor test`() {
        val integrationKey = "integrationKey"
        val token = "token"
        val appVersion = "appVersion"
        val sdkVersion = "sdkVersion"
        val deviceId = "deviceId"
        val advertisingId = "advertisingId"
        val carrierId = "carrierId"
        val contactKey = "contactKey"
        val permission = true
        val trackingPermission = true
        val tokenType = "tokenType"
        val webSubscription = "webSubscription"
        val testGroup = "testGroup"
        val country = "country"
        val language = "language"
        val timezone = "timezone"

        val subscription = Subscription(
            integrationKey = integrationKey,
            token = token,
            appVersion = appVersion,
            sdkVersion = sdkVersion,
            deviceId = deviceId,
            advertisingId = advertisingId,
            carrierId = carrierId,
            contactKey = contactKey,
            permission = permission,
            trackingPermission = trackingPermission,
            tokenType = tokenType,
            webSubscription = webSubscription,
            testGroup = testGroup,
            country = country,
            language = language,
            timezone = timezone
        )

        Assert.assertEquals(integrationKey, subscription.integrationKey)
        Assert.assertEquals(token, subscription.token)
        Assert.assertEquals(appVersion, subscription.appVersion)
        Assert.assertEquals(sdkVersion, subscription.sdkVersion)
        Assert.assertEquals(deviceId, subscription.deviceId)
        Assert.assertEquals(advertisingId, subscription.advertisingId)
        Assert.assertEquals(carrierId, subscription.carrierId)
        Assert.assertEquals(contactKey, subscription.contactKey)
        Assert.assertEquals(permission, subscription.permission)
        Assert.assertEquals(trackingPermission, subscription.trackingPermission)
        Assert.assertEquals(tokenType, subscription.tokenType)
        Assert.assertEquals(webSubscription, subscription.webSubscription)
        Assert.assertEquals(testGroup, subscription.testGroup)
        Assert.assertEquals(country, subscription.country)
        Assert.assertEquals(language, subscription.language)
        Assert.assertEquals(timezone, subscription.timezone)
    }

}