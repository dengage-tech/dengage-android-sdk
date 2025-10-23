package com.dengage.sdk.domain.inappmessage


import com.dengage.sdk.data.remote.api.ApiType.REAL_TIME_INAPP
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.inappmessage.model.InAppMessageData


class GetRealTimeInAppRepository {

    private val service: GetRealTimeInAppService by service(REAL_TIME_INAPP)

    suspend fun getRealTimeInAppMessages(
        accountId: String,
        appId: String?,
    ): MutableList<InAppMessageData>? {
        return service.getRealTimeInAppMessages(
            accountId = accountId,
            appId = appId
        )
    }



}