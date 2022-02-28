package com.dengage.sdk.domain.tag

import com.dengage.sdk.domain.tag.model.TagsRequest
import retrofit2.Response
import retrofit2.http.Body
import retrofit2.http.Headers
import retrofit2.http.POST

interface TagService {

    @Headers("CONNECT_TIMEOUT:10000", "READ_TIMEOUT:10000", "WRITE_TIMEOUT:10000")
    @POST("/api/setTags")
    suspend fun setTags(
        @Body request: TagsRequest
    ): Response<Unit>
}
