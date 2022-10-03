package com.dengage.sdk.domain.inappmessage

import com.dengage.sdk.data.remote.api.ApiType.IN_APP
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.inappmessage.model.InAppMessageData
import com.dengage.sdk.domain.subscription.model.Subscription
import retrofit2.Response

class RealTimeInAppMessageRepository {

    private val service: InAppMessageService by service(IN_APP)

    suspend fun getRealTimeInAppMessages(
        accountId: String,
        appId: String?,
    ): MutableList<InAppMessageData>? {
        return service.getRealTimeInAppMessages(
            accountId = accountId,
            appId = appId
        )
    }

    suspend fun setRealTimeInAppMessageAsDisplayed(
        accountName: String?,
        subscription: Subscription,
        appId: String?,
        sessionId: String,
        campaignId: String,
        messageDetails: String?
    ): Response<Unit> {
        return service.setRealTimeInAppMessageAsDisplayed(
            accountName = accountName,
            contactKey = subscription.contactKey,
            deviceId = subscription.getSafeDeviceId(),
            appId = appId,
            sessionId = sessionId,
            campaignId = campaignId,
            messageDetails = messageDetails
        )
    }

    suspend fun setRealTimeInAppMessageAsClicked(
        accountName: String?,
        subscription: Subscription,
        appId: String?,
        sessionId: String,
        campaignId: String,
        messageDetails: String?,
        buttonId: String?
    ): Response<Unit> {
        return service.setRealTimeInAppMessageAsClicked(
            accountName = accountName,
            contactKey = subscription.contactKey,
            deviceId = subscription.getSafeDeviceId(),
            appId = appId,
            sessionId = sessionId,
            campaignId = campaignId,
            messageDetails = messageDetails,
            buttonId = buttonId
        )
    }

    suspend fun setRealTimeInAppMessageAsDismissed(
        accountName: String?,
        subscription: Subscription,
        appId: String?,
        sessionId: String,
        campaignId: String,
        messageDetails: String?
    ): Response<Unit> {
        return service.setRealTimeInAppMessageAsDismissed(
            accountName = accountName,
            contactKey = subscription.contactKey,
            deviceId = subscription.getSafeDeviceId(),
            appId = appId,
            sessionId = sessionId,
            campaignId = campaignId,
            messageDetails = messageDetails
        )
    }

}
