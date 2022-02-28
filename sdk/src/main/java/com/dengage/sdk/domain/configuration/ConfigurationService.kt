package com.dengage.sdk.domain.configuration

import com.dengage.sdk.domain.configuration.model.SdkParameters
import retrofit2.http.GET
import retrofit2.http.Headers
import retrofit2.http.Query

interface ConfigurationService {

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @GET("/api/getSdkParams")
    suspend fun getSdkParameters(
        @Query("ik") integrationKey: String
    ): SdkParameters

}