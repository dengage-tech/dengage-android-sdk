package com.dengage.sdk.domain.tag

import com.dengage.sdk.data.remote.api.ApiType
import com.dengage.sdk.data.remote.api.service
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.domain.tag.model.TagsRequest
import retrofit2.Response

class TagRepository {

    private val service: TagService by service(ApiType.PUSH)

    suspend fun setTags(
        account: String,
        subscription: Subscription,
        tags: List<TagItem>
    ): Response<Unit> {
        return service.setTags(
            request = TagsRequest(
                accountName = account,
                key = subscription.getSafeDeviceId(),
                tags = tags
            )
        )
    }
}
