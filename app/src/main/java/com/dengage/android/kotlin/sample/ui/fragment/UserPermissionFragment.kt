package com.dengage.android.kotlin.sample.ui.fragment

import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentUserPermissionBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.Dengage

class UserPermissionFragment : BaseDataBindingFragment<FragmentUserPermissionBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_user_permission
    }

    override fun init() {
        sendPageView("user-permission")

        binding.swUserPermission.text = getString(
            R.string.dengage_user_permission,
            Dengage.getUserPermission().toString()
        )

        binding.swUserPermission.isChecked = Dengage.getUserPermission() ?: false
        binding.swUserPermission.setOnCheckedChangeListener { _, isChecked ->
            Dengage.setUserPermission(
                permission = isChecked
            )

            binding.swUserPermission.text = getString(
                R.string.dengage_user_permission,
                Dengage.getUserPermission().toString()
            )
        }
    }

}