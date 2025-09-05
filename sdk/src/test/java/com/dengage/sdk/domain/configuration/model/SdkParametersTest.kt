package com.dengage.sdk.domain.configuration.model

import org.junit.Assert
import org.junit.Test

class SdkParametersTest {

    @Test
    fun `SdkParameters constructor test`() {
        val appId = "1"
        val accountId = 1
        val accountName = "accountName"
        val eventsEnabled = true
        val inboxEnabled = true
        val inAppEnabled = true
        val subscriptionEnabled = false
        val inAppFetchIntervalInMin = 1
        val inAppMinSecBetweenMessages = 1
        val lastFetchTimeInMillis = 3L
        val appTrackingEnabled = true
        val appTrackingList = null
        val realTimeInAppEnabled = true
        val realTimeInAppFetchIntervalInMinutes = 30
        val realTimeInAppSessionTimeoutMinutes = 30
        val expiredMessagesFetchIntervalInMin = 30
        val surveyCheckEndpoint = "endpoint"
        val eventMappings = listOf(EventMapping(
            eventTableName = "eventTableName",
            enableClientHistory = true,
            clientHistoryOptions = ClientHistoryOptions(maxEventCount = 3, timeWindowInMinutes = 30),
            eventTypeDefinitions = listOf(EventTypeDefinition(
                eventTypeId = 30,
                eventType = "eventType",
                logicOperator = "AND",
                filterConditions = listOf(FilterCondition(
                    fieldName = "fieldName",
                    operator = "OR",
                    values = listOf("value1", "value2")
                ))
            ))
        ))

        val sdkParameters = SdkParameters(
            appId = appId,
            accountId = accountId,
            accountName = accountName,
            eventsEnabled = eventsEnabled,
            inboxEnabled = inboxEnabled,
            inAppEnabled = inAppEnabled,
            subscriptionEnabled = subscriptionEnabled,
            inAppFetchIntervalInMin = inAppFetchIntervalInMin,
            inAppMinSecBetweenMessages = inAppMinSecBetweenMessages,
            lastFetchTimeInMillis = lastFetchTimeInMillis,
            appTrackingEnabled = appTrackingEnabled,
            appTrackingList = appTrackingList,
            realTimeInAppEnabled = realTimeInAppEnabled,
            realTimeInAppFetchIntervalInMinutes = realTimeInAppFetchIntervalInMinutes,
            realTimeInAppSessionTimeoutMinutes = realTimeInAppSessionTimeoutMinutes,
            expiredMessagesFetchIntervalInMin = expiredMessagesFetchIntervalInMin,
            surveyCheckEndpoint = surveyCheckEndpoint,
            eventMappings = eventMappings,
        )

        Assert.assertEquals(appId, sdkParameters.appId)
        Assert.assertEquals(accountId, sdkParameters.accountId)
        Assert.assertEquals(accountName, sdkParameters.accountName)
        Assert.assertEquals(eventsEnabled, sdkParameters.eventsEnabled)
        Assert.assertEquals(inboxEnabled, sdkParameters.inboxEnabled)
        Assert.assertEquals(inAppEnabled, sdkParameters.inAppEnabled)
        Assert.assertEquals(subscriptionEnabled, sdkParameters.subscriptionEnabled)
        Assert.assertEquals(inAppFetchIntervalInMin, sdkParameters.inAppFetchIntervalInMin)
        Assert.assertEquals(inAppMinSecBetweenMessages, sdkParameters.inAppMinSecBetweenMessages)
        Assert.assertEquals(lastFetchTimeInMillis, sdkParameters.lastFetchTimeInMillis)
        Assert.assertEquals(appTrackingEnabled, sdkParameters.appTrackingEnabled)
        Assert.assertEquals(appTrackingList, sdkParameters.appTrackingList)
        Assert.assertEquals(realTimeInAppEnabled, sdkParameters.realTimeInAppEnabled)
        Assert.assertEquals(realTimeInAppFetchIntervalInMinutes, sdkParameters.realTimeInAppFetchIntervalInMinutes)
        Assert.assertEquals(realTimeInAppSessionTimeoutMinutes, sdkParameters.realTimeInAppSessionTimeoutMinutes)
        Assert.assertEquals(expiredMessagesFetchIntervalInMin, sdkParameters.expiredMessagesFetchIntervalInMin)
        Assert.assertEquals(surveyCheckEndpoint, sdkParameters.surveyCheckEndpoint)
        Assert.assertEquals(eventMappings, sdkParameters.eventMappings)
    }

}