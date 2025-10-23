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

    // ...existing code...

    @Test
    fun `operateCartFilter should count items with has_promotion filter`() {
        val cartItems = listOf(
            createCartItem("123", hasPromotion = true),
            createCartItem("456", hasPromotion = false),
            createCartItem("789", hasPromotion = true),
            createCartItem("999", hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("2"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Should count 2 items with has_promotion = true
    }

    @Test
    fun `operateCartFilter should return false when promotion count criteria not met`() {
        val cartItems = listOf(
            createCartItem("123", hasPromotion = false),
            createCartItem("456", hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result) // Should count 0 items with has_promotion = true, not equal to 1
    }

    @Test
    fun `operateCartFilter should handle has_promotion filter with edge cases`() {
        val cartItems = listOf(
            createCartItem("123", hasPromotion = true),
            createCartItem("456", hasPromotion = false),
            createCartItem("789", hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Should count only 1 item with has_promotion = true
    }

    @Test
    fun `operateCartFilter should count distinct product_ids correctly`() {
        val cartItems = listOf(
            createCartItem("product_123"),
            createCartItem("product_456"),
            createCartItem("product_789"),
            createCartItem("product_123") // duplicate product_id
        )
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_THAN",
            values = listOf("2"),
            filters = emptyList()
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Should count 3 distinct product_ids, which is > 2
    }

    @Test
    fun `operateCartFilter should return false when distinct count criteria not met`() {
        val cartItems = listOf(
            createCartItem("product_123"),
            createCartItem("product_123"), // duplicate
            createCartItem("product_456")
        )
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_THAN",
            values = listOf("2"),
            filters = emptyList()
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result) // Should count 2 distinct product_ids, which is not > 2
    }

    @Test
    fun `operateCartFilter should handle distinct count with empty cart - scenario`() {
        Prefs.clientCart = Cart(emptyList())

        val criterion = createCriterion(
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_THAN",
            values = listOf("2"),
            filters = emptyList()
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result) // Should count 0 distinct product_ids, which is not > 2
    }

    @Test
    fun `operateCartFilter should handle distinct count with empty product_ids`() {
        val cartItems = listOf(
            createCartItem("product_123"),
            createCartItem("null"),
            createCartItem("product_456"),
            createCartItem("") // empty product_id
        )
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_THAN",
            values = listOf("2"),
            filters = emptyList()
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Should count distinct non-null values: "product_123", "product_456", ""
    }

    @Test
    fun `operateCartFilter should test exact boundary conditions for has_promotion`() {
        // Test exactly 1 item with promotion
        val cartItems = listOf(
            createCartItem("123", hasPromotion = true),
            createCartItem("456", hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Should count exactly 1 item with has_promotion = true
    }

    @Test
    fun `operateCartFilter should test exact boundary conditions`() {
        // Test exactly 3 distinct product_ids
        val cartItems = listOf(
            createCartItem("product_A"),
            createCartItem("product_B"),
            createCartItem("product_C")
        )
        Prefs.clientCart = Cart(cartItems)

        val criterion = createCriterion(
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_THAN",
            values = listOf("2"),
            filters = emptyList()
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Should count 3 distinct product_ids, which is > 2
    }

    @Test
    fun `operateCartFilter should handle boolean string variations`() {
        val cartItems = listOf(
            createCartItem("123", hasPromotion = true),
            createCartItem("456", hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        // Test with different boolean string representations
        val filters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("TRUE")) // uppercase
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Should handle case-insensitive boolean comparison
    }

    @Test
    fun `operateCartFilter should handle mixed scenarios with complex cart`() {
        val cartItems = listOf(
            createCartItem("product_A", hasPromotion = true),
            createCartItem("product_B", hasPromotion = false),
            createCartItem("product_A", hasPromotion = true), // duplicate product_id with promotion
            createCartItem("product_C", hasPromotion = true),
            createCartItem("product_D", hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        // Test scenario: Count items with promotion
        val promotionFilters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val promotionCriterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("3"),
            filters = promotionFilters,
            filtersLogicalOp = "AND"
        )

        val promotionResult = CartUtils.operateCartFilter(promotionCriterion)
        Assert.assertTrue(promotionResult) // Should count 3 items with promotion

        // Test scenario: Count distinct product_ids
        val distinctCriterion = createCriterion(
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_THAN",
            values = listOf("2"),
            filters = emptyList()
        )

        val distinctResult = CartUtils.operateCartFilter(distinctCriterion)
        Assert.assertTrue(distinctResult) // Should count 4 distinct product_ids (A, B, C, D), which is > 2
    }

    @Test
    fun `operateCartFilter should handle with NOT_EQUALS operator`() {
        val cartItems = listOf(
            createCartItem("123", hasPromotion = true),
            createCartItem("456", hasPromotion = false),
            createCartItem("789", hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("has_promotion", "NOT_EQUALS", "BOOL", listOf("true"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("2"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Should count 2 items where has_promotion != true
    }

    @Test
    fun `operateCartFilter should handle scenario with different operators`() {
        val cartItems = listOf(
            createCartItem("product_A"),
            createCartItem("product_B"),
            createCartItem("product_C")
        )
        Prefs.clientCart = Cart(cartItems)

        // Test with EQUALS operator
        val equalsCriterion = createCriterion(
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "EQUALS",
            values = listOf("3"),
            filters = emptyList()
        )

        val equalsResult = CartUtils.operateCartFilter(equalsCriterion)
        Assert.assertTrue(equalsResult) // Should count exactly 3 distinct product_ids

        // Test with GREATER_EQUAL operator
        val greaterEqualCriterion = createCriterion(
            aggregateType = "DISTINCT_COUNT",
            aggregateField = "product_id",
            operator = "GREATER_EQUAL",
            values = listOf("3"),
            filters = emptyList()
        )

        val greaterEqualResult = CartUtils.operateCartFilter(greaterEqualCriterion)
        Assert.assertTrue(greaterEqualResult) // Should count 3 distinct product_ids, which is >= 3
    }

    @Test
    fun `operateCartFilter should count items with promotion filter equals 1`() {
        val cartItems = listOf(
            createCartItem("123", hasPromotion = true),
            createCartItem("456", hasPromotion = false),
            createCartItem("789", hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Should count exactly 1 item with has_promotion = true
    }

    @Test
    fun `operateCartFilter should return false when promotion count not equals 1`() {
        val cartItems = listOf(
            createCartItem("123", hasPromotion = true),
            createCartItem("456", hasPromotion = true), // 2 items with promotion
            createCartItem("789", hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result) // Should count 2 items with promotion, not equal to 1
    }

    @Test
    fun `operateCartFilter should handle zero promotion items`() {
        val cartItems = listOf(
            createCartItem("123", hasPromotion = false),
            createCartItem("456", hasPromotion = false),
            createCartItem("789", hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("1"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result) // Should count 0 items with promotion, not equal to 1
    }

    @Test
    fun `operateCartFilter should count zero kozmetik items without discount`() {
        val cartItems = listOf(
            createCartItem("123", categoryPath = "giyim/erkek", hasDiscount = false),
            createCartItem("456", categoryPath = "elektronik/telefon", hasDiscount = false),
            createCartItem("789", categoryPath = "ev/mutfak", hasDiscount = true)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik")),
            EventFilter("has_discount", "EQUALS", "BOOL", listOf("false"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("0"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Should count 0 items matching both criteria
    }

    @Test
    fun `operateCartFilter should return false when kozmetik items without discount exist`() {
        val cartItems = listOf(
            createCartItem("123", categoryPath = "kozmetik/parfum", hasDiscount = false),
            createCartItem("456", categoryPath = "giyim/erkek", hasDiscount = false),
            createCartItem("789", categoryPath = "kozmetik/makyaj", hasDiscount = true) // has discount, should be filtered out
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik")),
            EventFilter("has_discount", "EQUALS", "BOOL", listOf("false"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("0"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result) // Should count 1 item (kozmetik/parfum without discount), not equal to 0
    }

    @Test
    fun `operateCartFilter should handle mixed kozmetik items`() {
        val cartItems = listOf(
            createCartItem("123", categoryPath = "kozmetik/parfum", hasDiscount = true), // has discount
            createCartItem("456", categoryPath = "kozmetik/makyaj", hasDiscount = true), // has discount
            createCartItem("789", categoryPath = "giyim/erkek", hasDiscount = false), // not kozmetik
            createCartItem("999", categoryPath = "kozmetik/cildBakimi", hasDiscount = false) // has no discount
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik")),
            EventFilter("has_discount", "EQUALS", "BOOL", listOf("false"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("0"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result) // Should count 1 item (kozmetik/cildBakimi without discount), not equal to 0
    }

    @Test
    fun `operateCartFilter should handle case sensitivity in category path`() {
        val cartItems = listOf(
            createCartItem("123", categoryPath = "KOZMETIK/PARFUM", hasDiscount = false),
            createCartItem("456", categoryPath = "Kozmetik/Makyaj", hasDiscount = false),
            createCartItem("789", categoryPath = "giyim/erkek", hasDiscount = false)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik")),
            EventFilter("has_discount", "EQUALS", "BOOL", listOf("false"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("0"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result) // Should count 2 items (case-insensitive matching), not equal to 0
    }

    @Test
    fun `operateCartFilter should handle empty cart`() {
        Prefs.clientCart = Cart(emptyList())

        val filters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik")),
            EventFilter("has_discount", "EQUALS", "BOOL", listOf("false"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("0"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // Empty cart should count 0 items
    }

    @Test
    fun `operateCartFilter should handle partial kozmetik matches`() {
        val cartItems = listOf(
            createCartItem("123", categoryPath = "kozmetikUrunleri/parfum", hasDiscount = false), // contains kozmetik
            createCartItem("456", categoryPath = "giyim/kozmetikAksesuari", hasDiscount = false), // contains kozmetik
            createCartItem("789", categoryPath = "ev/mutfak", hasDiscount = false) // does not contain kozmetik
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik")),
            EventFilter("has_discount", "EQUALS", "BOOL", listOf("false"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("0"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertFalse(result) // Should count 2 items containing "kozmetik" without discount, not equal to 0
    }

    @Test
    fun `operateCartFilter should handle empty category path`() {
        val cartItems = listOf(
            createCartItem("123", categoryPath = "", hasDiscount = false),
            createCartItem("456", categoryPath = "", hasDiscount = false),
            createCartItem("789", categoryPath = "kozmetik/parfum", hasDiscount = true)
        )
        Prefs.clientCart = Cart(cartItems)

        val filters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik")),
            EventFilter("has_discount", "EQUALS", "BOOL", listOf("false"))
        )

        val criterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("0"),
            filters = filters,
            filtersLogicalOp = "AND"
        )

        val result = CartUtils.operateCartFilter(criterion)
        Assert.assertTrue(result) // No items should match (null/empty paths don't contain "kozmetik")
    }

    @Test
    fun `operateCartFilter should handle both scenarios in same cart`() {
        val cartItems = listOf(
            createCartItem("123", categoryPath = "kozmetik/parfum", hasDiscount = false, hasPromotion = true),
            createCartItem("456", categoryPath = "giyim/erkek", hasDiscount = false, hasPromotion = false),
            createCartItem("789", categoryPath = "kozmetik/makyaj", hasDiscount = true, hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        // Test scenario: Count items with promotion = 1
        val promotionFilters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val promotionCriterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("1"),
            filters = promotionFilters,
            filtersLogicalOp = "AND"
        )

        val promotionResult = CartUtils.operateCartFilter(promotionCriterion)
        Assert.assertTrue(promotionResult) // Should count 1 item with promotion

        // Test scenario: Count kozmetik items without discount = 0
        val kozmetikFilters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik")),
            EventFilter("has_discount", "EQUALS", "BOOL", listOf("false"))
        )

        val kozmetikCriterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("0"),
            filters = kozmetikFilters,
            filtersLogicalOp = "AND"
        )

        val kozmetikResult = CartUtils.operateCartFilter(kozmetikCriterion)
        Assert.assertFalse(kozmetikResult) // Should count 1 kozmetik item without discount (kozmetik/parfum), not equal to 0
    }

    @Test
    fun `operateCartFilter should handle edge case with exact scenario requirements`() {
        val cartItems = listOf(
            createCartItem("123", categoryPath = "giyim/erkek", hasDiscount = false, hasPromotion = true),
            createCartItem("456", categoryPath = "kozmetik/parfum", hasDiscount = false, hasPromotion = false)
        )
        Prefs.clientCart = Cart(cartItems)

        // Test scenario: exactly 1 item with promotion
        val promotionFilters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val promotionCriterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("1"),
            filters = promotionFilters,
            filtersLogicalOp = "AND"
        )

        val promotionResult = CartUtils.operateCartFilter(promotionCriterion)
        Assert.assertTrue(promotionResult) // Should pass

        // Test scenario: 0 kozmetik items without discount
        val kozmetikFilters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik")),
            EventFilter("has_discount", "EQUALS", "BOOL", listOf("false"))
        )

        val kozmetikCriterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("0"),
            filters = kozmetikFilters,
            filtersLogicalOp = "AND"
        )

        val kozmetikResult = CartUtils.operateCartFilter(kozmetikCriterion)
        Assert.assertFalse(kozmetikResult) // Should fail (1 kozmetik item without discount exists)
    }

    @Test
    fun `operateCartFilter should handle perfect scenario satisfaction`() {
        // Create cart that satisfies both scenarios
        val cartItems = listOf(
            createCartItem("123", categoryPath = "giyim/erkek", hasDiscount = false, hasPromotion = true),
            createCartItem("456", categoryPath = "kozmetik/parfum", hasDiscount = true, hasPromotion = false), // kozmetik with discount
            createCartItem("789", categoryPath = "elektronik/telefon", hasDiscount = false, hasPromotion = false) // unrelated item
        )
        Prefs.clientCart = Cart(cartItems)

        // Test scenario: exactly 1 item with promotion
        val promotionFilters = listOf(
            EventFilter("has_promotion", "EQUALS", "BOOL", listOf("true"))
        )

        val promotionCriterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("1"),
            filters = promotionFilters,
            filtersLogicalOp = "AND"
        )

        val promotionResult = CartUtils.operateCartFilter(promotionCriterion)
        Assert.assertTrue(promotionResult) // Should pass

        // Test scenario: 0 kozmetik items without discount
        val kozmetikFilters = listOf(
            EventFilter("category_path", "LIKE", "TEXT", listOf("kozmetik")),
            EventFilter("has_discount", "EQUALS", "BOOL", listOf("false"))
        )

        val kozmetikCriterion = createCriterion(
            aggregateType = "COUNT",
            operator = "EQUALS",
            values = listOf("0"),
            filters = kozmetikFilters,
            filtersLogicalOp = "AND"
        )

        val kozmetikResult = CartUtils.operateCartFilter(kozmetikCriterion)
        Assert.assertTrue(kozmetikResult) // Should pass (no kozmetik items without discount)
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
        productVariantId: String = "",
        categoryPath: String = "",
        price: Int = 0,
        discountedPrice: Int = 0,
        hasDiscount: Boolean = false,
        hasPromotion: Boolean = false,
        quantity: Int = 0,
        attributes: Map<String, String> = emptyMap()
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