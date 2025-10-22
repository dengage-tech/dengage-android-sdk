package com.dengage.sdk.domain.inappmessage

import retrofit2.Response
import retrofit2.http.*

interface CouponService {

    @Headers("CONNECT_TIMEOUT:5000", "READ_TIMEOUT:5000", "WRITE_TIMEOUT:5000")
    @POST("/coupon/{accountId}/assign")
    suspend fun assignCoupon(
        @Path("accountId") accountId: String,
        @Body request: CouponAssignRequest
    ): Response<CouponAssignResponse>
}

data class CouponAssignRequest(
    val key: String
)

data class CouponAssignResponse(
    val code: String
)
