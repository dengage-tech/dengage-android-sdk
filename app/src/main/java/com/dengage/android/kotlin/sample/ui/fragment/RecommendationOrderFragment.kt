package com.dengage.android.kotlin.sample.ui.fragment

import android.widget.Toast
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentRecommendationOrderBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage
import org.json.JSONObject
import kotlin.random.Random

/**
 * Opened when a "dengage://recommendationOrder" or "dengagesample://recommendationOrder"
 * deeplink arrives. Shows the product id carried by the deeplink and sends an order
 * for it to order_events and order_events_detail via Dengage.sendDeviceEvent(...).
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

            val quantity = binding.etQuantity.text.toString().toIntOrNull() ?: 1
            val unitPrice = binding.etUnitPrice.text.toString().toDoubleOrNull() ?: 0.0
            val totalAmount = unitPrice * quantity
            val paymentMethod = binding.etPaymentMethod.text.toString()
            val orderId = Random.nextInt(1_000_000, 10_000_000)

            val dataOrderEvents = hashMapOf<String, Any>(
                "order_id" to orderId,
                "item_count" to quantity,
                "total_amount" to totalAmount.toNumber(),
                "payment_method" to paymentMethod,
                "shipping" to 1,
                "discounted_price" to totalAmount.toNumber(),
                "coupon_code" to binding.etCouponCode.text.toString(),
                "event_type" to "order"
            )

            val dataOrderEventsDetail = hashMapOf<String, Any>(
                "product_id" to productId,
                "product_variant_id" to "1",
                "quantity" to quantity,
                "unit_price" to unitPrice.toNumber().toString(),
                "discounted_price" to unitPrice.toNumber().toString(),
                "order_id" to orderId,
                "payment_method" to paymentMethod,
                "event_type" to "order"
            )

            // sendDeviceEvent adds session_id, dn_device_id and dn_contact_key to these maps,
            // so the payload text is built before sending.
            binding.tvResult.text = buildString {
                append("Sent order ").append(orderId).append("\n\n")
                append("order_events:\n").append(JSONObject(dataOrderEvents as Map<*, *>).toString(2))
                append("\n\norder_events_detail:\n").append(JSONObject(dataOrderEventsDetail as Map<*, *>).toString(2))
            }

            Dengage.sendDeviceEvent("order_events", dataOrderEvents, requireContext())
            Dengage.sendDeviceEvent("order_events_detail", dataOrderEventsDetail, requireContext())

            Toast.makeText(
                requireContext(),
                "Order sent for product $productId",
                Toast.LENGTH_SHORT
            ).show()
        }
    }

    /** Whole prices go out as integers (995000), like the panel's sample payload. */
    private fun Double.toNumber(): Number = if (this % 1.0 == 0.0) toLong() else this

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
