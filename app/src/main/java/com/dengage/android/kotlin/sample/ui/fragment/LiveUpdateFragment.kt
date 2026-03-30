package com.dengage.android.kotlin.sample.ui.fragment

import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentLiveUpdateBinding
import com.dengage.android.kotlin.sample.liveupdate.DeliveryLiveUpdateHandler
import com.dengage.android.kotlin.sample.liveupdate.SportsLiveUpdateHandler
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.sdk.liveupdate.LiveUpdateEvent
import com.dengage.sdk.liveupdate.LiveUpdatePayload

class LiveUpdateFragment : BaseDataBindingFragment<FragmentLiveUpdateBinding>() {

    override fun getLayoutRes() = R.layout.fragment_live_update

    private val deliveryHandler = DeliveryLiveUpdateHandler()
    private val sportsHandler = SportsLiveUpdateHandler()

    private val deliveryActivityId = "demo-delivery-001"
    private val sportsActivityId = "demo-sports-001"

    private var score1 = 0
    private var score2 = 0
    private var matchTime = "1'"
    private var period = "1. Yarı"

    override fun init() {
        setupDeliveryTemplate()
        setupSportsTemplate()
    }

    // -------------------------------------------------------------------------
    // Delivery
    // -------------------------------------------------------------------------

    private fun sendDelivery(event: LiveUpdateEvent, status: String, eta: String) {
        deliveryHandler.onUpdate(
            requireContext(),
            LiveUpdatePayload(
                activityType = "delivery",
                event = event,
                activityId = deliveryActivityId,
                contentState = mapOf(
                    "order_id" to "DNG-8821",
                    "delivery_status" to status,
                    "estimated_time" to eta
                )
            )
        )
    }

    private fun setupDeliveryTemplate() {
        binding.btnDeliveryStep1.setOnClickListener { sendDelivery(LiveUpdateEvent.START, "ORDER_RECEIVED", "45 dk") }
        binding.btnDeliveryStep2.setOnClickListener { sendDelivery(LiveUpdateEvent.UPDATE, "PREPARING", "30 dk") }
        binding.btnDeliveryStep3.setOnClickListener { sendDelivery(LiveUpdateEvent.UPDATE, "ON_THE_WAY", "10 dk") }
        binding.btnDeliveryStep4.setOnClickListener { sendDelivery(LiveUpdateEvent.UPDATE, "DELIVERED", "") }
        binding.btnDeliveryDismiss.setOnClickListener {
            deliveryHandler.onUpdate(
                requireContext(),
                LiveUpdatePayload("delivery", LiveUpdateEvent.END, deliveryActivityId, emptyMap())
            )
        }
    }

    // -------------------------------------------------------------------------
    // Sports
    // -------------------------------------------------------------------------

    private fun sendSports(event: LiveUpdateEvent = LiveUpdateEvent.UPDATE) {
        sportsHandler.onUpdate(
            requireContext(),
            LiveUpdatePayload(
                activityType = "sports",
                event = event,
                activityId = sportsActivityId,
                contentState = mapOf(
                    "team1" to "Galatasaray",
                    "team2" to "Fenerbahçe",
                    "score1" to score1.toString(),
                    "score2" to score2.toString(),
                    "match_time" to matchTime,
                    "period" to period
                )
            )
        )
    }

    private fun setupSportsTemplate() {
        binding.btnSportsStart.setOnClickListener {
            score1 = 0; score2 = 0; matchTime = "1'"; period = "1. Yarı"
            sendSports(LiveUpdateEvent.START)
        }
        binding.btnSportsGoal1.setOnClickListener { score1++; matchTime = nextTime(); sendSports() }
        binding.btnSportsGoal2.setOnClickListener { score2++; matchTime = nextTime(); sendSports() }
        binding.btnSportsHalfTime.setOnClickListener { matchTime = "45'"; period = "2. Yarı"; sendSports() }
        binding.btnSportsFullTime.setOnClickListener { matchTime = "90'"; period = "Maç Bitti"; sendSports() }
        binding.btnSportsDismiss.setOnClickListener {
            sportsHandler.onUpdate(
                requireContext(),
                LiveUpdatePayload("sports", LiveUpdateEvent.END, sportsActivityId, emptyMap())
            )
        }
    }

    private fun nextTime(): String {
        val current = matchTime.removeSuffix("'").toIntOrNull() ?: 1
        return "${(current + 10).coerceAtMost(90)}'"
    }
}
