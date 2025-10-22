package com.dengage.android.kotlin.sample.ui.adapter

import android.view.View
import android.view.ViewGroup
import androidx.appcompat.widget.AppCompatButton
import androidx.appcompat.widget.AppCompatEditText
import androidx.appcompat.widget.AppCompatCheckBox
import androidx.appcompat.widget.AppCompatTextView
import androidx.core.widget.addTextChangedListener
import androidx.recyclerview.widget.RecyclerView
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.ui.base.BaseRecyclerViewAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseRecyclerViewHolder
import com.dengage.sdk.domain.inappmessage.model.CartItem

class CartItemsAdapter(
    items: List<CartItem> = arrayListOf(),
    private val onRemoveItem: (position: Int) -> Unit
) : BaseRecyclerViewAdapter(items) {
    
    override fun createNewViewHolder(parent: ViewGroup, viewType: Int) =
        ViewHolder(parent)

    inner class ViewHolder(
        parent: ViewGroup
    ) : BaseRecyclerViewHolder<CartItem>(parent, R.layout.recycler_item_cart) {

        private val etProductId = itemView.findViewById<AppCompatEditText>(R.id.etProductId)
        private val etProductVariantId = itemView.findViewById<AppCompatEditText>(R.id.etProductVariantId)
        private val etCategoryPath = itemView.findViewById<AppCompatEditText>(R.id.etCategoryPath)
        private val etPrice = itemView.findViewById<AppCompatEditText>(R.id.etPrice)
        private val etDiscountedPrice = itemView.findViewById<AppCompatEditText>(R.id.etDiscountedPrice)
        private val etQuantity = itemView.findViewById<AppCompatEditText>(R.id.etQuantity)
        private val cbHasDiscount = itemView.findViewById<AppCompatCheckBox>(R.id.cbHasDiscount)
        private val cbHasPromotion = itemView.findViewById<AppCompatCheckBox>(R.id.cbHasPromotion)
        private val tvEffectivePrice = itemView.findViewById<AppCompatTextView>(R.id.tvEffectivePrice)
        private val tvLineTotal = itemView.findViewById<AppCompatTextView>(R.id.tvLineTotal)
        private val tvDiscountedLineTotal = itemView.findViewById<AppCompatTextView>(R.id.tvDiscountedLineTotal)
        private val tvEffectiveLineTotal = itemView.findViewById<AppCompatTextView>(R.id.tvEffectiveLineTotal)
        private val btnRemove = itemView.findViewById<AppCompatButton>(R.id.btnRemoveItem)

        private var isUpdating = false

        override fun bindItem(item: CartItem) {
            isUpdating = true
            
            etProductId.setText(item.productId)
            etProductVariantId.setText(item.productVariantId)
            etCategoryPath.setText(item.categoryPath)
            etPrice.setText(item.price.toString())
            etDiscountedPrice.setText(item.discountedPrice.toString())
            etQuantity.setText(item.quantity.toString())
            cbHasDiscount.isChecked = item.hasDiscount
            cbHasPromotion.isChecked = item.hasPromotion

            // Display calculated fields (read-only)
            updateCalculatedFields(item)

            isUpdating = false

            // Set up text change listeners to update the item
            etProductId.addTextChangedListener {
                if (!isUpdating) updateCartItem()
            }
            
            etProductVariantId.addTextChangedListener {
                if (!isUpdating) updateCartItem()
            }
            
            etCategoryPath.addTextChangedListener {
                if (!isUpdating) updateCartItem()
            }
            
            etPrice.addTextChangedListener {
                if (!isUpdating) updateCartItem()
            }
            
            etDiscountedPrice.addTextChangedListener {
                if (!isUpdating) updateCartItem()
            }
            
            etQuantity.addTextChangedListener {
                if (!isUpdating) updateCartItem()
            }
            
            cbHasDiscount.setOnCheckedChangeListener { _, _ ->
                if (!isUpdating) updateCartItem()
            }
            
            cbHasPromotion.setOnCheckedChangeListener { _, _ ->
                if (!isUpdating) updateCartItem()
            }

            btnRemove.setOnClickListener {
                onRemoveItem(adapterPosition)
            }
        }
        
        private fun updateCartItem() {
            val position = adapterPosition
            if (position != RecyclerView.NO_POSITION && position >= 0 && position < itemList.size) {
                val updatedItem = createUpdatedCartItem()
                itemList[position] = updatedItem
                updateCalculatedFields(updatedItem)
            }
        }
        
        private fun createUpdatedCartItem(): CartItem {
            return CartItem(
                productId = etProductId.text.toString(),
                productVariantId = etProductVariantId.text.toString(),
                categoryPath = etCategoryPath.text.toString(),
                price = etPrice.text.toString().toIntOrNull() ?: 0,
                discountedPrice = etDiscountedPrice.text.toString().toIntOrNull() ?: 0,
                hasDiscount = cbHasDiscount.isChecked,
                hasPromotion = cbHasPromotion.isChecked,
                quantity = etQuantity.text.toString().toIntOrNull() ?: 0,
                attributes = emptyMap()
            )
        }
        
        private fun updateCalculatedFields(item: CartItem) {
            tvEffectivePrice.text = "Effective Price: ${item.effectivePrice}"
            tvLineTotal.text = "Line Total: ${item.lineTotal}"
            tvDiscountedLineTotal.text = "Discounted Line Total: ${item.discountedLineTotal}"
            tvEffectiveLineTotal.text = "Effective Line Total: ${item.effectiveLineTotal}"
        }
    }
}
