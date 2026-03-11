package com.dengage.android.kotlin.sample.ui.fragment

import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentLiveUpdateBinding
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.liveupdate.DengageLiveUpdateManager
import com.dengage.sdk.liveupdate.DengageLiveUpdateManager.DeliveryStatus
import com.dengage.sdk.liveupdate.DengageLiveUpdateManager.DeliveryUpdate
import com.dengage.sdk.liveupdate.DengageLiveUpdateManager.SportsUpdate

class LiveUpdateFragment : BaseDataBindingFragment<FragmentLiveUpdateBinding>() {

    override fun getLayoutRes() = R.layout.fragment_live_update

    private var score1 = 0
    private var score2 = 0
    private var matchTime = "1'"
    private var period = "1. Yarı"

    override fun init() {
        setupDeliveryTemplate()
        setupSportsTemplate()
    }

    private fun setupDeliveryTemplate() {
        binding.btnDeliveryStep1.setOnClickListener {
            DengageLiveUpdateManager.showDeliveryUpdate(
                requireContext(),
                DeliveryUpdate(orderId = "DNG-8821", status = DeliveryStatus.ORDER_RECEIVED, estimatedTime = "45 dk")
            )
        }

        binding.btnDeliveryStep2.setOnClickListener {
            DengageLiveUpdateManager.showDeliveryUpdate(
                requireContext(),
                DeliveryUpdate(orderId = "DNG-8821", status = DeliveryStatus.PREPARING, estimatedTime = "30 dk")
            )
        }

        binding.btnDeliveryStep3.setOnClickListener {
            DengageLiveUpdateManager.showDeliveryUpdate(
                requireContext(),
                DeliveryUpdate(orderId = "DNG-8821", status = DeliveryStatus.ON_THE_WAY, estimatedTime = "10 dk")
            )
        }

        binding.btnDeliveryStep4.setOnClickListener {
            DengageLiveUpdateManager.showDeliveryUpdate(
                requireContext(),
                DeliveryUpdate(orderId = "DNG-8821", status = DeliveryStatus.DELIVERED, estimatedTime = "")
            )
        }

        binding.btnDeliveryDismiss.setOnClickListener {
            DengageLiveUpdateManager.dismissDeliveryUpdate(requireContext())
        }
    }

    private fun setupSportsTemplate() {
        binding.btnSportsStart.setOnClickListener {
            score1 = 0; score2 = 0; matchTime = "1'"; period = "1. Yarı"
            showSportsUpdate()
        }

        binding.btnSportsGoal1.setOnClickListener {
            score1++; matchTime = nextTime(); showSportsUpdate()
        }

        binding.btnSportsGoal2.setOnClickListener {
            score2++; matchTime = nextTime(); showSportsUpdate()
        }

        binding.btnSportsHalfTime.setOnClickListener {
            matchTime = "45'"; period = "2. Yarı"; showSportsUpdate()
        }

        binding.btnSportsFullTime.setOnClickListener {
            matchTime = "90'"; period = "Maç Bitti"; showSportsUpdate()
        }

        binding.btnSportsDismiss.setOnClickListener {
            DengageLiveUpdateManager.dismissSportsUpdate(requireContext())
        }
    }

    private fun showSportsUpdate() {
        DengageLiveUpdateManager.showSportsUpdate(
            requireContext(),
            SportsUpdate(
                team1 = "Galatasaray",
                team2 = "Fenerbahçe",
                score1 = score1,
                score2 = score2,
                matchTime = matchTime,
                period = period
            )
        )
    }

    private fun nextTime(): String {
        val current = matchTime.removeSuffix("'").toIntOrNull() ?: 1
        return "${(current + 10).coerceAtMost(90)}'"
    }
}
