package com.dengage.sdk.domain.tag.usecase

import com.dengage.sdk.domain.base.CoroutineUseCase
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.domain.tag.TagRepository
import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.util.createLazy
import retrofit2.Response

class SetTags : CoroutineUseCase<Response<Unit>, SetTags.Params>() {

    private val repository: TagRepository by createLazy()

    override suspend fun buildUseCase(params: Params?): Response<Unit> =
        repository.setTags(
            account = params!!.account,
            subscription = params.subscription,
            tags = params.tags
        )

    data class Params(
        val account: String,
        val subscription: Subscription,
        val tags: List<TagItem>
    )
}
