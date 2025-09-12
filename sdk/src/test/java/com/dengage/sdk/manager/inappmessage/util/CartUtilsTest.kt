package com.dengage.sdk.manager.inappmessage.util

import androidx.test.platform.app.InstrumentationRegistry
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.Cart
import com.dengage.sdk.domain.inappmessage.model.CartItem
import com.dengage.sdk.domain.inappmessage.model.Criterion
import com.dengage.sdk.domain.inappmessage.model.EventFilter
import com.dengage.sdk.util.ContextHolder
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

@Config(manifest = Config.NONE)
@RunWith(RobolectricTestRunner::class)
class CartUtilsTest {

    @Before
    fun setup() {
        ContextHolder.context = InstrumentationRegistry.getInstrumentation().context
        // Clear cart before each test
        Prefs.clientCart = null
    }

    @Test
    fun `operateCartFilter should return false when cart is null`() {
        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "GT",
            values = listOf("1")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateCartFilter should return false when cart is empty`() {
        Prefs.clientCart = Cart(emptyList())

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "GT",
            values = listOf("0")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateCartFilter should count items correctly`() {
        val cartItems = listOf(
            createCartItem("123", price = 1000),
            createCartItem("456", price = 2000),
            createCartItem("789", price = 3000)
        )
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("3")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should count distinct items correctly`() {
        val cartItems = listOf(
            createCartItem("123", price = 1000),
            createCartItem("456", price = 2000),
            createCartItem("123", price = 1000) // duplicate product_id
        )
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "EQ",
            values = listOf("2")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should sum effective prices correctly`() {
        val cartItems = listOf(
            createCartItem("123", price = 1000, hasDiscount = false),
            createCartItem("456", price = 2000, discountedPrice = 1500, hasDiscount = true),
            createCartItem("789", price = 3000, hasDiscount = false)
        )
        Prefs.clientCart = Cart(cartItems)

        // Sum should be: 1000 + 1500 + 3000 = 5500
        val criterion = createCriterion(
            aggregateType = "SUM",
            aggregateField = "effective_price",
            operator = "EQ",
            values = listOf("5500")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should find minimum price correctly`() {
        val cartItems = listOf(
            createCartItem("123", price = 1000),
            createCartItem("456", price = 500),
            createCartItem("789", price = 3000)
        )
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "MIN",
            aggregateField = "price",
            operator = "EQ",
            values = listOf("500")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should find maximum price correctly`() {
        val cartItems = listOf(
            createCartItem("123", price = 1000),
            createCartItem("456", price = 500),
            createCartItem("789", price = 3000)
        )
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "MAX",
            aggregateField = "price",
            operator = "EQ",
            values = listOf("3000")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should calculate average price correctly`() {
        val cartItems = listOf(
            createCartItem("123", price = 1000),
            createCartItem("456", price = 2000),
            createCartItem("789", price = 3000)
        )
        Prefs.clientCart = Cart(cartItems)

        // Average should be: (1000 + 2000 + 3000) / 3 = 2000
        val criterion = createCriterion(
            aggregateType = "AVG",
            aggregateField = "price",
            operator = "EQ",
            values = listOf("2000")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should apply EQUALS filter correctly`() {
        val cartItems = listOf(
            createCartItem("123", hasDiscount = false),
            createCartItem("456", hasDiscount = true),
            createCartItem("789", hasDiscount = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("has_discount", "EQUALS", "TEXT", listOf("false"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("2"),
            filters = filters
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should apply GREATER_THAN filter correctly`() {
        val cartItems = listOf(
            createCartItem("123", price = 500),
            createCartItem("456", price = 1500),
            createCartItem("789", price = 2500)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("price", "GREATER_THAN", "TEXT", listOf("1000"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("2"),
            filters = filters
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should apply CONTAINS_ALL filter correctly`() {
        val cartItems = listOf(
            createCartItem("123", categoryPath = "kozmetik/parfum/erkek"),
            createCartItem("456", categoryPath = "kozmetik/makyaj"),
            createCartItem("789", categoryPath = "giyim/erkek")
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("category_path", "CONTAINS_ALL", "TEXT", listOf("kozmetik", "parfum"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should apply attribute filters correctly`() {
        val cartItems = listOf(
            createCartItem("123", attributes = mapOf("brand" to "VakkoBeauty")),
            createCartItem("456", attributes = mapOf("brand" to "WCollection")),
            createCartItem("789", attributes = mapOf("brand" to "VakkoBeauty"))
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("attributes.brand", "EQUALS", "TEXT", listOf("VakkoBeauty"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("2"),
            filters = filters
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should apply AND logic correctly`() {
        val cartItems = listOf(
            createCartItem("123", price = 1500, hasDiscount = false),
            createCartItem("456", price = 2500, hasDiscount = true),
            createCartItem("789", price = 500, hasDiscount = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("price", "GREATER_THAN", "TEXT", listOf("1000")),
            EventFilter("has_discount", "EQUALS", "TEXT", listOf("false"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should apply OR logic correctly`() {
        val cartItems = listOf(
            createCartItem("123", price = 500, hasDiscount = false),
            createCartItem("456", price = 2500, hasDiscount = true),
            createCartItem("789", price = 1500, hasDiscount = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("price", "GREATER_THAN", "TEXT", listOf("2000")),
            EventFilter("has_discount", "EQUALS", "TEXT", listOf("false"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("3"),
            filters = filters,
            filtersLogicalOp = "OR"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should test all comparison operators`() {
        val cartItems = listOf(
            createCartItem("123", price = 1000),
            createCartItem("456", price = 2000),
            createCartItem("789", price = 3000)
        )
        Prefs.clientCart = Cart(cartItems)

        // Test GREATER_THAN
        var criterion = createCriterion("COUNT", "GREATER_THAN", listOf("2"))
        Assert.assertTrue(CartUtils.operateCartFilter(criterion))

        // Test GREATER_EQUAL
        criterion = createCriterion("COUNT", "GREATER_EQUAL", listOf("3"))
        Assert.assertTrue(CartUtils.operateCartFilter(criterion))

        // Test LESS_THAN
        criterion = createCriterion("COUNT", "LESS_THAN", listOf("4"))
        Assert.assertTrue(CartUtils.operateCartFilter(criterion))

        // Test LESS_EQUAL
        criterion = createCriterion("COUNT", "LESS_EQUAL", listOf("3"))
        Assert.assertTrue(CartUtils.operateCartFilter(criterion))

        // Test NOT_EQUALS
        criterion = createCriterion("COUNT", "NOT_EQUALS", listOf("2"))
        Assert.assertTrue(CartUtils.operateCartFilter(criterion))
    }

    @Test
    fun `operateCartFilter should handle invalid aggregate type`() {
        val cartItems = listOf(createCartItem("123", price = 1000))
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "INVALID_TYPE",
            operator = "EQ",
            values = listOf("1")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateCartFilter should handle invalid operator`() {
        val cartItems = listOf(createCartItem("123", price = 1000))
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "INVALID_OP",
            values = listOf("1")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateCartFilter should handle null values`() {
        val cartItems = listOf(createCartItem("123", price = 1000))
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQ",
            values = null
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateCartFilter should handle non-numeric values`() {
        val cartItems = listOf(createCartItem("123", price = 1000))
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("not_a_number")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateCartFilter should handle missing aggregate field for DISTINCT_COUNT`() {
        val cartItems = listOf(createCartItem("123", price = 1000))
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "DISTINCT_COUNT",
            aggregateField = null,
            operator = "EQ",
            values = listOf("1")
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result)
    }

    @Test
    fun `operateCartFilter should handle string comparisons case insensitively`() {
        val cartItems = listOf(
            createCartItem("123", categoryPath = "Kozmetik/Parfum")
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQ",
            values = listOf("1"),
            filters = filters
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result)
    }

    @Test
    fun `operateCartFilter should handle exceptions gracefully`() {
        // Create malformed criterion that might cause exceptions
        val criterion = Criterion(
            id = 1,
            parameter = "cart_items",
            dataType = "INT",
            operator = "EQ",
            values = listOf("invalid"),
            valueSource = "BROWSER",
            aggregateType = "INVALID",
            aggregateField = null,
            eventType = null,
            timeWindow = null,
            filtersLogicalOp = null,
            filters = null
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result) // Should handle gracefully and return false
    }

    // Helper methods
    private fun createCriterion(
        aggregateType: String,
        operator: String,
        values: List<String>?,
        aggregateField: String? = null,
        filters: List<EventFilter>? = null,
        filtersLogicalOp: String? = null
    ): Criterion {
        return Criterion(
            id = 1,
            parameter = "cart_items",
            dataType = "INT",
            operator = operator,
            values = values,
            valueSource = "BROWSER",
            aggregateType = aggregateType,
            aggregateField = aggregateField,
            eventType = null,
            timeWindow = null,
            filtersLogicalOp = filtersLogicalOp,
            filters = filters
        )
    }

    private fun createCartItem(
        productId: String,
        productVariantId: String? = null,
        categoryPath: String? = null,
        price: Int? = null,
        discountedPrice: Int? = null,
        hasDiscount: Boolean? = null,
        hasPromotion: Boolean? = null,
        quantity: Int? = 1,
        attributes: Map<String, String>? = null
    ): CartItem {
        return CartItem(
            productId = productId,
            productVariantId = productVariantId,
            categoryPath = categoryPath,
            price = price,
            discountedPrice = discountedPrice,
            hasDiscount = hasDiscount,
            hasPromotion = hasPromotion,
            quantity = quantity,
            attributes = attributes
        )
    }
}