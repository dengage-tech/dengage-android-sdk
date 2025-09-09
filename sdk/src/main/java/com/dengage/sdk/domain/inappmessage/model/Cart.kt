package com.dengage.sdk.domain.inappmessage.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class Cart(
    @SerializedName("items") val items: List<CartItem>,
    @SerializedName("summary") val summary: CartSummary = CartSummary.calculate(items)
) : Serializable

data class CartItem(
    @SerializedName("product_id") val productId: String?,
    @SerializedName("product_variant_id") val productVariantId: String?,
    @SerializedName("category_path") val categoryPath: String?,
    @SerializedName("price") val price: Int?,
    @SerializedName("discounted_price") val discountedPrice: Int?,
    @SerializedName("has_discount") val hasDiscount: Boolean?,
    @SerializedName("has_promotion") val hasPromotion: Boolean?,
    @SerializedName("quantity") val quantity: Int?,
    @SerializedName("attributes") val attributes: Map<String, String>?,
    // Calculated fields - these will be computed by the SDK
    @SerializedName("effective_price") val effectivePrice: Int = calculateEffectivePrice(price, discountedPrice, hasDiscount, hasPromotion),
    @SerializedName("line_total") val lineTotal: Int = calculateLineTotal(price, quantity),
    @SerializedName("discounted_line_total") val discountedLineTotal: Int = calculateDiscountedLineTotal(discountedPrice, quantity),
    @SerializedName("effective_line_total") val effectiveLineTotal: Int = calculateEffectiveLineTotal(price, discountedPrice, hasDiscount, hasPromotion, quantity),
    // Normalized fields
    @SerializedName("category_segments") val categorySegments: List<String> = parseCategorySegments(categoryPath),
    @SerializedName("category_root") val categoryRoot: String = getCategoryRootFromPath(categoryPath)
) : Serializable {

    companion object {
        
        private fun calculateEffectivePrice(price: Int?, discountedPrice: Int?, hasDiscount: Boolean?, hasPromotion: Boolean?): Int {
            return if ((hasDiscount == true || hasPromotion == true) && discountedPrice != null) {
                discountedPrice
            } else {
                price ?: 0
            }
        }
        
        private fun calculateLineTotal(price: Int?, quantity: Int?): Int {
            return (price ?: 0) * (quantity ?: 0)
        }
        
        private fun calculateDiscountedLineTotal(discountedPrice: Int?, quantity: Int?): Int {
            return (discountedPrice ?: 0) * (quantity ?: 0)
        }
        
        private fun calculateEffectiveLineTotal(
            price: Int?, 
            discountedPrice: Int?, 
            hasDiscount: Boolean?,
            hasPromotion: Boolean?,
            quantity: Int?
        ): Int {
            val effectivePrice = calculateEffectivePrice(price, discountedPrice, hasDiscount, hasPromotion)
            return effectivePrice * (quantity ?: 0)
        }
        
        private fun parseCategorySegments(categoryPath: String?): List<String> {
            return categoryPath?.split("/")?.filter { it.isNotEmpty() } ?: emptyList()
        }
        
        private fun getCategoryRootFromPath(categoryPath: String?): String {
            return parseCategorySegments(categoryPath).firstOrNull() ?: ""
        }
    }
}

data class CartSummary(
    @SerializedName("currency") val currency: String = "TRY",
    @SerializedName("updatedAt") val updatedAt: Long = System.currentTimeMillis(),
    
    // Count fields
    @SerializedName("lines_count") val linesCount: Int,
    @SerializedName("items_count") val itemsCount: Int,
    
    // Total fields
    @SerializedName("subtotal") val subtotal: Int,
    @SerializedName("discounted_subtotal") val discountedSubtotal: Int,
    @SerializedName("effective_subtotal") val effectiveSubtotal: Int,
    
    // Discount flags
    @SerializedName("any_discounted") val anyDiscounted: Boolean,
    @SerializedName("all_discounted") val allDiscounted: Boolean,
    
    // Price ranges
    @SerializedName("min_price") val minPrice: Int,
    @SerializedName("max_price") val maxPrice: Int,
    @SerializedName("min_effective_price") val minEffectivePrice: Int,
    @SerializedName("max_effective_price") val maxEffectivePrice: Int,
    
    // Category breakdown
    @SerializedName("categories") val categories: Map<String, Int>
) : Serializable {
    
    companion object {
        fun calculate(items: List<CartItem>): CartSummary {
            if (items.isEmpty()) {
                return CartSummary(
                    linesCount = 0,
                    itemsCount = 0,
                    subtotal = 0,
                    discountedSubtotal = 0,
                    effectiveSubtotal = 0,
                    anyDiscounted = false,
                    allDiscounted = false,
                    minPrice = 0,
                    maxPrice = 0,
                    minEffectivePrice = 0,
                    maxEffectivePrice = 0,
                    categories = emptyMap()
                )
            }
            
            // Count calculations
            val linesCount = items.size
            val itemsCount = items.sumOf { it.quantity ?: 0 }
            
            // Total calculations
            val subtotal = items.sumOf { it.lineTotal }
            val discountedSubtotal = items.sumOf { it.discountedLineTotal }
            val effectiveSubtotal = items.sumOf { it.effectiveLineTotal }
            
            // Discount flags
            val anyDiscounted = items.any { it.hasDiscount == true || it.hasPromotion == true }
            val allDiscounted = items.all { it.hasDiscount == true || it.hasPromotion == true }
            
            // Price ranges
            val prices = items.mapNotNull { it.price }
            val effectivePrices = items.map { it.effectivePrice }
            
            val minPrice = prices.minOrNull() ?: 0
            val maxPrice = prices.maxOrNull() ?: 0
            val minEffectivePrice = effectivePrices.minOrNull() ?: 0
            val maxEffectivePrice = effectivePrices.maxOrNull() ?: 0
            
            // Category breakdown
            val categories = mutableMapOf<String, Int>()
            items.forEach { item ->
                val categoryRoot = item.categoryRoot
                if (categoryRoot.isNotEmpty()) {
                    categories[categoryRoot] = (categories[categoryRoot] ?: 0) + (item.quantity ?: 0)
                }
            }
            
            return CartSummary(
                linesCount = linesCount,
                itemsCount = itemsCount,
                subtotal = subtotal,
                discountedSubtotal = discountedSubtotal,
                effectiveSubtotal = effectiveSubtotal,
                anyDiscounted = anyDiscounted,
                allDiscounted = allDiscounted,
                minPrice = minPrice,
                maxPrice = maxPrice,
                minEffectivePrice = minEffectivePrice,
                maxEffectivePrice = maxEffectivePrice,
                categories = categories
            )
        }
    }
}