package com.dengage.sdk.manager.configuration

import com.dengage.sdk.domain.configuration.model.SdkParameters
import com.dengage.sdk.manager.base.BasePresenter
import com.dengage.sdk.manager.base.BaseView

interface ConfigurationContract {

    interface View : BaseView {
        fun sdkParametersFetched(sdkParameters: SdkParameters)
    }

    interface Presenter : BasePresenter<View> {
        fun getSdkParameters(
            integrationKey: String
        )
    }
}