package com.dengage.sdk.manager.inappmessage.util

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.Cart
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

}
