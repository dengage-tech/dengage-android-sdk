package com.dengage.sdk.domain.inappmessage.model

import java.io.Serializable

data class ClientPageInfo(
    val lastProductId: String? = null,
    val lastProductPrice: String? = null,
    val lastCategoryPath: String? = null,
    val currentPageTitle: String? = null,
    val currentPageType: String? = null
) : Serializable
