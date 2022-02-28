package com.dengage.sdk.manager.tag

import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.manager.base.BasePresenter
import com.dengage.sdk.manager.base.BaseView

interface TagContract {

    interface View : BaseView {
        fun tagsSent()
    }

    interface Presenter : BasePresenter<View> {
        fun setTags(
            tags: List<TagItem>
        )
    }
}