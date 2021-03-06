package com.dengage.sdk.domain.inappmessage

import com.dengage.sdk.data.remote.api.ApiType.PUSH
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.util.extension.getCdKey
import com.dengage.sdk.util.extension.getType
import retrofit2.Response

class InAppMessageRepository {

    private val service: InAppMessageService by service(PUSH)

    suspend fun getInAppMessages(
        account: String,
        subscription: Subscription
    ): MutableList<InAppMessage>? {
        return service.getInAppMessages(
            account = account,
            cdKey = subscription.getCdKey(),
            deviceId = subscription.getSafeDeviceId(),
            type = subscription.getType()
        )
    }

    suspend fun setInAppMessageAsDisplayed(
        account: String,
        subscription: Subscription,
        messageDetails: String?
    ): Response<Unit> {
        return service.setInAppMessageAsDisplayed(
            account = account,
            cdKey = subscription.getCdKey(),
            deviceId = subscription.getSafeDeviceId(),
            type = subscription.getType(),
            messageDetails = messageDetails
        )
    }

    suspend fun setInAppMessageAsClicked(
        account: String,
        subscription: Subscription,
        messageDetails: String?,
        buttonId: String?
    ): Response<Unit> {
        return service.setInAppMessageAsClicked(
            account = account,
            cdKey = subscription.getCdKey(),
            deviceId = subscription.getSafeDeviceId(),
            type = subscription.getType(),
            messageDetails = messageDetails,
            buttonId = buttonId
        )
    }

    suspend fun setInAppMessageAsDismissed(
        account: String,
        subscription: Subscription,
        messageDetails: String?
    ): Response<Unit> {
        return service.setInAppMessageAsDismissed(
            account = account,
            cdKey = subscription.getCdKey(),
            deviceId = subscription.getSafeDeviceId(),
            type = subscription.getType(),
            messageDetails = messageDetails
        )
    }

}
