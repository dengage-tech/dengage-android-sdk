package com.dengage.sdk.domain.inappmessage.usecase

import android.util.Base64
import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.inappmessage.CouponAssignRequest
import com.dengage.sdk.domain.inappmessage.CouponAssignResponse
import com.dengage.sdk.domain.inappmessage.InAppMessageRepository
import org.json.JSONObject
import retrofit2.Response

class AssignCoupon : CoroutineUseCase<Response<CouponAssignResponse>, AssignCoupon.Params>() {

    private val repository by lazy { InAppMessageRepository() }

    override suspend fun buildUseCase(params: Params?): Response<CouponAssignResponse> {
        val jsonObject = JSONObject().apply {
            put("ListKey", params!!.listKey)
            put("ContactKey", params.contactKey)
            put("DeviceId", params.deviceId)
            put("CampaignId", params.campaignId)
        }

        val jsonString = jsonObject.toString()
        val encodedKey = Base64.encodeToString(jsonString.toByteArray(), Base64.NO_WRAP)

        val request = CouponAssignRequest(key = encodedKey)

        return repository.assignCoupon(
            accountId = params!!.accountId,
            request = request
        )
    }

    data class Params(
        val accountId: String,
        val listKey: String,
        val contactKey: String,
        val deviceId: String,
        val campaignId: String
    )
}