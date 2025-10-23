package com.dengage.sdk.manager.inappmessage.util

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.Cart
import com.dengage.sdk.domain.inappmessage.model.ClientPageInfo
import com.dengage.sdk.util.DengageLogger

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

    fun setCart(cart: Cart) {
        try {
            Prefs.clientCart = cart

            DengageLogger.debug("Cart stored: ${cart.items.size} items")
        } catch (e: Exception) {
            DengageLogger.error("Error storing cart: ${e.message}")
        }
    }

    fun getCart(): Cart {
        return Prefs.clientCart ?: Cart(emptyList())
    }

    fun setClientPageInfo(eventDetails: Map<String, Any>) {
        try {
            val currentClientPageInfo = Prefs.clientPageInfo ?: ClientPageInfo()

            var updatedClientPageInfo = currentClientPageInfo.copy(
                currentPageTitle = eventDetails["page_title"]?.toString() ?: currentClientPageInfo.currentPageTitle,
                currentPageType = eventDetails["page_type"]?.toString() ?: currentClientPageInfo.currentPageType
            )

            // If page_type is "product", update last product information
            if (eventDetails["page_type"]?.toString() == "product") {
                updatedClientPageInfo = updatedClientPageInfo.copy(
                    lastProductId = eventDetails["product_id"]?.toString() ?: currentClientPageInfo.lastProductId,
                    lastProductPrice = eventDetails["price"]?.toString() ?: currentClientPageInfo.lastProductPrice,
                    lastCategoryPath = eventDetails["category_path"]?.toString() ?: currentClientPageInfo.lastCategoryPath
                )
            }

            Prefs.clientPageInfo = updatedClientPageInfo

            DengageLogger.debug("Client page info updated: $updatedClientPageInfo")
        } catch (e: Exception) {
            DengageLogger.error("Error setting client page info: ${e.message}")
        }
    }

    fun getClientPageInfo(): ClientPageInfo {
        return Prefs.clientPageInfo ?: ClientPageInfo()
    }

    fun getLastProductId(): String? {
        return getClientPageInfo().lastProductId
    }

    fun getLastProductPrice(): String? {
        return getClientPageInfo().lastProductPrice
    }

    fun getLastCategoryPath(): String? {
        return getClientPageInfo().lastCategoryPath
    }

    fun getCurrentPageTitle(): String? {
        return getClientPageInfo().currentPageTitle
    }

    fun getCurrentPageType(): String? {
        return getClientPageInfo().currentPageType
    }

}
