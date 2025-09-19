package com.dengage.android.kotlin.sample.ui.fragment

import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentCartBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class CartFragment : BaseDataBindingFragment<FragmentCartBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_cart
    }

    override fun init() {


        binding.btnSetCart.setOnClickListener {
            //Dengage.setCart()
            //Dengage.sendCustomEvent("view_cart", hashMapOf())
            //showToast("Cart viewed")
        }
    }
}
