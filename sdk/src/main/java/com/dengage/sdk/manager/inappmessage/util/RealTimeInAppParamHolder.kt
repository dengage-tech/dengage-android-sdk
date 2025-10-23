package com.dengage.sdk.manager.inappmessage.util

object RealTimeInAppParamHolder {

    var categoryPath: String? = null
    var cartItemCount: String? = null
    var cartAmount: String? = null
    var state: String? = null
    var city: String? = null
    var pageViewVisitCount: Int = 0

    fun addPageView() {
        pageViewVisitCount++
    }

}
