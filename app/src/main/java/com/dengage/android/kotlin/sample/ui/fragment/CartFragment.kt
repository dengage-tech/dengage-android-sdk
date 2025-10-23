package com.dengage.android.kotlin.sample.ui.fragment

import android.widget.Toast
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentCartBinding
import com.dengage.android.kotlin.sample.ui.adapter.CartItemsAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage
import com.dengage.sdk.domain.inappmessage.model.Cart
import com.dengage.sdk.domain.inappmessage.model.CartItem

class CartFragment : BaseDataBindingFragment<FragmentCartBinding>() {

    private lateinit var adapter: CartItemsAdapter
    private val cartItems = mutableListOf<CartItem>()

    override fun getLayoutRes(): Int {
        return R.layout.fragment_cart
    }

    override fun init() {
        setupRecyclerView()
        setupClickListeners()
        loadCurrentCart()
    }

    private fun setupRecyclerView() {
        adapter = CartItemsAdapter(cartItems) { position ->
            removeCartItem(position)
        }
        binding.rvCartItems.adapter = adapter
    }

    private fun setupClickListeners() {
        binding.btnAddItem.setOnClickListener {
            addNewCartItem()
        }

        binding.btnUpdateCart.setOnClickListener {
            updateCart()
        }
    }

    private fun loadCurrentCart() {
        try {
            val currentCart = Dengage.getCart()
            cartItems.clear()
            cartItems.addAll(currentCart.items)
            
            // Debug logging
            showToast("Loading cart: ${currentCart.items.size} items")
            
            // Ensure proper casting and update
            adapter.setItems(cartItems.map { it as Any })
            adapter.notifyDataSetChanged()
            
        } catch (e: Exception) {
            showToast("Error loading cart: ${e.message}")
        }
    }

    private fun addNewCartItem() {
        val newItem = CartItem(
            productId = "",
            productVariantId = "",
            categoryPath = "",
            price = 0,
            discountedPrice = 0,
            hasDiscount = false,
            hasPromotion = false,
            quantity = 1,
            attributes = emptyMap()
        )
        cartItems.add(newItem)
        adapter.addItem(newItem as Any, cartItems.size - 1)
        showToast("New item added")
    }

    private fun removeCartItem(position: Int) {
        if (position >= 0 && position < cartItems.size) {
            cartItems.removeAt(position)
            adapter.removeItem(position)
            showToast("Item removed")
        }
    }

    private fun updateCart() {
        try {
            // Get updated items from adapter
            val updatedItems = adapter.getItems().filterIsInstance<CartItem>()
            
            // Debug: Check if all items are retrieved
            showToast("Retrieved ${updatedItems.size} items from adapter")
            
            // Create new cart with updated items
            val newCart = Cart(updatedItems)
            
            // Update cart in SDK
            Dengage.setCart(newCart)
            
            // Update local cart items
            cartItems.clear()
            cartItems.addAll(updatedItems)
            
            showToast("Cart updated successfully!")
            
        } catch (e: Exception) {
            showToast("Error updating cart: ${e.message}")
        }
    }

    private fun showToast(message: String) {
        Toast.makeText(requireContext(), message, Toast.LENGTH_SHORT).show()
    }
}
