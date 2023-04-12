package com.dengage.sdk.manager.configuration

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.configuration.usecase.GetSdkParameters
import com.dengage.sdk.manager.base.BaseAbstractPresenter
import com.dengage.sdk.util.DengageLogger

class ConfigurationPresenter : BaseAbstractPresenter<ConfigurationContract.View>(),
    ConfigurationContract.Presenter {

    private val getSdkParameters by lazy { GetSdkParameters() }

    override fun getSdkParameters(integrationKey: String) {
        getSdkParameters(this) {
            onResponse = {
                it.lastFetchTimeInMillis = System.currentTimeMillis()
                Prefs.sdkParameters = it
                view { sdkParametersFetched(it) }
            }
            onError = {
                DengageLogger.error(it.toString())

            }
            params = GetSdkParameters.Params(
                integrationKey = integrationKey
            )
        }
    }
}
