package com.dengage.sdk.manager.tag

import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.manager.base.BaseMvpManager

class TagManager : BaseMvpManager<TagContract.View, TagContract.Presenter>(),
    TagContract.View {

    override fun providePresenter() = TagPresenter()

    internal fun setTags(tags: List<TagItem>) {
        presenter.setTags(
            tags = tags
        )
    }

    override fun tagsSent() = Unit

}
