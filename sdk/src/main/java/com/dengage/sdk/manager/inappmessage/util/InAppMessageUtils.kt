package com.dengage.sdk.manager.inappmessage.util

import android.content.Context
import android.util.TypedValue
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.model.Operator
import com.dengage.sdk.domain.inappmessage.model.TriggerBy
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger
import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.*

object InAppMessageUtils {

    /**
     * Find not expired in app messages with controlling expire date and date now
     *
     * @param inAppMessages in app messages that will be filtered with expire date
     */
    fun findNotExpiredInAppMessages(
        untilDate: Date,
        inAppMessages: List<InAppMessage>?
    ): MutableList<InAppMessage>? {
        if (inAppMessages == null) return null
        val notExpiredMessages = mutableListOf<InAppMessage>()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        expireDateFormat.timeZone = TimeZone.getTimeZone("UTC")
        for (inAppMessage in inAppMessages) {
            try {
                val expireDate = expireDateFormat.parse(inAppMessage.data.expireDate)
                if (untilDate.before(expireDate)) {
                    notExpiredMessages.add(inAppMessage)
                }
            } catch (e: ParseException) {
                DengageLogger.error("expireDateFormatError: " + e.message)
                e.printStackTrace()
            }
        }
        return notExpiredMessages
    }

    /**
     * Find prior in app message to show with respect to priority and expireDate parameters
     */
    fun findPriorInAppMessage(
        inAppMessages: List<InAppMessage>,
        screenName: String? = null
    ): InAppMessage? {
        // sort list with comparator
        val sortedInAppMessages = inAppMessages.sortedWith(InAppMessageComparator())

        // if screen name is empty, find in app message that has no screen name filter
        // if screen name is not empty, find in app message that screen name filter has screen name value
        // if screen name is not empty and could not found in app message with screen name filter, use in app message without screen name filter
        // Also control nextDisplayTime for showEveryXMinutes type in app messages
        val inAppMessageWithoutScreenName = sortedInAppMessages.firstOrNull { inAppMessage: InAppMessage ->
            inAppMessage.data.displayTiming.triggerBy != TriggerBy.EVENT.triggerBy &&
                inAppMessage.data.displayCondition.screenNameFilters.isNullOrEmpty() &&
                isDisplayTimeAvailable(inAppMessage)
        }
        return if (screenName.isNullOrEmpty()) {
            inAppMessageWithoutScreenName
        } else {
            val inAppMessageWithScreenName = sortedInAppMessages.firstOrNull { inAppMessage: InAppMessage ->
                inAppMessage.data.displayTiming.triggerBy != TriggerBy.EVENT.triggerBy &&
                    inAppMessage.data.displayCondition.screenNameFilters?.firstOrNull { screenNameFilter ->
                        operateScreenValues(
                            screenNameFilter.value,
                            screenName,
                            screenNameFilter.operator
                        )
                    } != null && isDisplayTimeAvailable(inAppMessage)
            }
            inAppMessageWithScreenName ?: inAppMessageWithoutScreenName
        }
    }

    fun operateScreenValues(
        screenNameFilterValue: List<String>,
        screenName: String,
        operator: String
    ): Boolean {
        val screenNameFilterValueSafe = screenNameFilterValue.firstOrNull() ?: ""
        when (operator) {
            Operator.EQUALS.operator -> {
                return screenNameFilterValueSafe == screenName
            }
            Operator.NOT_EQUALS.operator -> {
                return screenNameFilterValueSafe != screenName
            }
            Operator.LIKE.operator -> {
                return screenName.contains(screenNameFilterValueSafe, true)
            }
            Operator.NOT_LIKE.operator -> {
                return !screenName.contains(screenNameFilterValueSafe, true)
            }
            Operator.STARTS_WITH.operator -> {
                return screenName.startsWith(screenNameFilterValueSafe, true)
            }
            Operator.NOT_STARTS_WITH.operator -> {
                return !screenName.startsWith(screenNameFilterValueSafe, true)
            }
            Operator.ENDS_WITH.operator -> {
                return screenName.endsWith(screenNameFilterValueSafe, true)
            }
            Operator.NOT_ENDS_WITH.operator -> {
                return !screenName.endsWith(screenNameFilterValueSafe, true)
            }
            Operator.IN.operator -> {
                return screenNameFilterValue.contains(screenName)
            }
            Operator.NOT_IN.operator -> {
                return !screenNameFilterValue.contains(screenName)
            }
        }
        return true
    }

    private fun isDisplayTimeAvailable(inAppMessage: InAppMessage): Boolean {
        return inAppMessage.data.displayTiming.showEveryXMinutes == null ||
            inAppMessage.data.displayTiming.showEveryXMinutes == 0 ||
            inAppMessage.data.nextDisplayTime <= System.currentTimeMillis()
    }

    fun pxToDp(px: Int?, context: Context): Float {
        return TypedValue.applyDimension(
            TypedValue.COMPLEX_UNIT_DIP,
            (px ?: 0).toFloat(),
            context.resources.displayMetrics
        )
    }

    fun getPixelsByPercentage(screenSize: Int, margin: Int?): Int {
        return (screenSize * (margin ?: 0)) / 100
    }

}
