package com.dengage.android.kotlin.sample.ui.fragment

import android.widget.Toast
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentCustomEventBinding
import com.dengage.android.kotlin.sample.ui.adapter.EventParametersAdapter
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.android.kotlin.sample.ui.model.EventParameter
import com.dengage.sdk.Dengage
import com.dengage.sdk.domain.inappmessage.model.*

class CustomEventFragment : BaseDataBindingFragment<FragmentCustomEventBinding>() {

    private lateinit var adapter: EventParametersAdapter
    private val eventParameters = mutableListOf<EventParameter>()

    override fun getLayoutRes(): Int = R.layout.fragment_custom_event


    override fun init() {
        //sendPageView("custom-events")
        setupRecyclerView()
        setupClickListeners()

        setEventTableName("order_events")
        addNewParameter("event_type", "order")
        addNewParameter("page_type", "category")

        setSampleCartData()
    }

    private fun setupRecyclerView() {
        adapter = EventParametersAdapter(eventParameters) { position ->
            removeParameter(position)
        }
        binding.rvEventParameters.adapter = adapter
    }

    private fun setupClickListeners() {
        binding.btnAddParameter.setOnClickListener {
            addNewParameter()
        }

        binding.btnSend.setOnClickListener {
            sendCustomEvent()
        }
    }

    private fun setEventTableName(tableName: String) {
        binding.etTableName.setText(tableName)
    }

    private fun addNewParameter(key: String = "", value: String = "") {
        val newParameter = EventParameter(key, value)
        eventParameters.add(newParameter)
        adapter.addItem(newParameter, eventParameters.size - 1)
    }

    private fun removeParameter(position: Int) {
        if (eventParameters.size > 1) {
            eventParameters.removeAt(position)
            adapter.removeItem(position)
        }
    }

    private fun sendCustomEvent() {
        val tableName = binding.etTableName.text.toString().trim()

        if (tableName.isEmpty()) {
            showToast("Please enter a table name")
            return
        }

        val eventData = HashMap<String, Any>()
        eventParameters.forEach { parameter ->
            if (parameter.key.isNotEmpty()) {
                eventData[parameter.key] = parameter.value
            }
        }



        Dengage.sendDeviceEvent(
            tableName,
            eventData
        )

        /*
        Dengage.sendCustomEvent(
            tableName,
            Dengage.getSubscription()?.contactKey!!,
            eventData
        )
        */

        //repeat(100) {

        //}


    }

    private fun setSampleCartData() {
        try {
            val cartItems = listOf(
                CartItem(
                    productId = "LIP-001",
                    productVariantId = "LIP-001-RED",
                    categoryPath = "cosmetics/makeup/lipstick",
                    price = 129900,
                    discountedPrice = 109900,
                    hasDiscount = true,
                    hasPromotion = false,
                    quantity = 2,
                    attributes = mapOf(
                        "brand" to "VakkoBeauty",
                        "collection_id" to "vk90",
                        "color" to "red",
                        "shade_code" to "R12",
                        "eligible" to "true"
                    )
                ),
                CartItem(
                    productId = "TSHIRT-123",
                    productVariantId = "TSHIRT-123-M",
                    categoryPath = "fashion/men/tshirts",
                    price = 499900,
                    discountedPrice = 499900,
                    hasDiscount = false,
                    hasPromotion = false,
                    quantity = 1,
                    attributes = mapOf(
                        "brand" to "WCollection",
                        "size" to "M",
                        "material" to "cotton",
                        "eligible" to "true"
                    )
                ),
                CartItem(
                    productId = "SHOES-456",
                    productVariantId = "SHOES-456-42",
                    categoryPath = "fashion/men/shoes",
                    price = 899900,
                    discountedPrice = 799900,
                    hasDiscount = true,
                    hasPromotion = true,
                    quantity = 1,
                    attributes = mapOf(
                        "brand" to "Nike",
                        "size" to "42",
                        "material" to "leather",
                        "eligible" to "false"
                    )
                )
            )


            //val oldCart = Dengage.getCart()

            // Create cart with items
            val cart = Cart(cartItems)

            // Set cart in Dengage SDK
            Dengage.setCart(cart)

            val newCart = Dengage.getCart()

            showToast("Sample cart data set with ${newCart.summary.itemsCount} items, total amount: ${newCart.summary.effectiveSubtotal}")

        } catch (e: Exception) {
            showToast("Error setting cart data: ${e.message}")
        }
    }

    fun showToast(message: String) {
        Toast.makeText(requireContext(), message, Toast.LENGTH_SHORT).show()
    }

}