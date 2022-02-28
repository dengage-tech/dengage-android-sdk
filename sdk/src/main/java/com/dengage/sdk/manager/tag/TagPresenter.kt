package com.dengage.sdk.manager.tag

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.domain.tag.usecase.SetTags
import com.dengage.sdk.manager.base.BaseAbstractPresenter

class TagPresenter : BaseAbstractPresenter<TagContract.View>(),
    TagContract.Presenter {

    private val setTags by lazy { SetTags() }

    override fun setTags(tags: List<TagItem>) {
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters

        if (subscription?.deviceId != null && sdkParameters?.accountName != null) {
            setTags(this) {
                onResponse = {
                    view { tagsSent() }
                }
                params = SetTags.Params(
                    account = sdkParameters.accountName,
                    subscription = subscription,
                    tags = tags
                )
            }
        }
    }

}
