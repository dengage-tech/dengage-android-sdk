package com.dengage.android.kotlin.sample.ui.fragment

import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentContactKeyBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class ContactKeyFragment : BaseDataBindingFragment<FragmentContactKeyBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_contact_key
    }

    override fun init() {
        sendPageView("contact-key")

        binding.etContactKey.setText(Dengage.getSubscription()?.contactKey)
        binding.switchPermission.isChecked = Dengage.getUserPermission() ?: false

        binding.btnSave.setOnClickListener {
            val contactKey = binding.etContactKey.text.toString().trim()
            Dengage.setContactKey(contactKey)
        }

        binding.switchPermission.setOnCheckedChangeListener { _, isChecked ->
            Dengage.setUserPermission(permission = isChecked)
        }
    }
}