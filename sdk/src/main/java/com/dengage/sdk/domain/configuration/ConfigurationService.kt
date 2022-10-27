package com.dengage.sdk.domain.configuration

import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.domain.configuration.model.VisitorInfo
import retrofit2.http.GET
import retrofit2.http.Headers
import retrofit2.http.Query

interface ConfigurationService {

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @GET("/api/getSdkParams")
    suspend fun getSdkParameters(
        @Query("ik") integrationKey: String
    ): SdkParameters

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @GET("/api/audience/visitor-info")
    suspend fun getVisitorInfo(
        @Query("acc") accountName: String?,
        @Query("ckey") contactKey: String?,
        @Query("did") deviceId: String,
    ): VisitorInfo

}