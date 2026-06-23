package com.dengage.android.kotlin.sample.ui.fragment

import android.widget.Toast
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentRecommendationOrderBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage
import java.util.UUID

/**
 * Opened when a "dengagesample://recommendationOrder" deeplink arrives.
 * Shows the product id carried by the deeplink and lets the user place an order
 * for it via Dengage.order(...).
 */
class RecommendationOrderFragment : BaseDataBindingFragment<FragmentRecommendationOrderBinding>() {

    private val productId: String
        get() = arguments?.getString(ARG_PRODUCT_ID).orEmpty()

    override fun getLayoutRes(): Int {
        return R.layout.fragment_recommendation_order
    }

    override fun init() {
        binding.tvProductId.text = "Product ID: $productId"

        binding.btnBuy.setOnClickListener {
            if (productId.isEmpty()) {
                Toast.makeText(requireContext(), "No product id", Toast.LENGTH_SHORT).show()
                return@setOnClickListener
            }

            val orderId = UUID.randomUUID().toString()

            val dataOrderEvents = hashMapOf<String, Any>(
                "coupon_code" to "",
                "discounted_price" to 1579,
                "event_type" to "order",
                "item_count" to 1,
                "order_id" to orderId,
                "payment_method" to "card",
                "shipping" to 1,
                "total_amount" to 1579,
                "unit_price" to 1579
            )

            val dataOrderEventsDetail = hashMapOf<String, Any>(
                "discounted_price" to 1579,
                "event_type" to "order",
                "order_id" to orderId,
                "payment_method" to "card",
                "product_id" to productId,
                "product_variant_id" to productId,
                "quantity" to 1,
                "unit_price" to 1579
            )

            //Dengage.order(data, requireContext())
            Dengage.sendDeviceEvent("order_events", dataOrderEvents, requireContext())
            Dengage.sendDeviceEvent("order_events_detail", dataOrderEventsDetail, requireContext())

            Toast.makeText(
                requireContext(),
                "Order sent for product $productId",
                Toast.LENGTH_SHORT
            ).show()
        }
    }

    companion object {
        private const val ARG_PRODUCT_ID = "productId"

        fun newInstance(productId: String): RecommendationOrderFragment {
            return RecommendationOrderFragment().apply {
                arguments = android.os.Bundle().apply {
                    putString(ARG_PRODUCT_ID, productId)
                }
            }
        }
    }
}
