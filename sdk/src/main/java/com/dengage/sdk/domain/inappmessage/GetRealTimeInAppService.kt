package com.dengage.sdk.domain.inappmessage

import com.dengage.sdk.domain.inappmessage.model.InAppMessageData
import retrofit2.http.GET
import retrofit2.http.Headers
import retrofit2.http.Path

interface GetRealTimeInAppService {

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @GET("{accountId}/{appId}/campaign.json")
    suspend fun getRealTimeInAppMessages(
        @Path("accountId") accountId: String,
        @Path("appId") appId: String?,
    ): MutableList<InAppMessageData>?

}