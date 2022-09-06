package com.dengage.sdk.manager.inappmessage.util

import android.annotation.SuppressLint
import android.content.Context
import android.content.res.Resources
import android.os.Build
import android.util.TypedValue
import androidx.core.text.isDigitsOnly
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.*
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger
import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.*
import java.util.concurrent.TimeUnit

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
        screenName: String? = null,
        params: HashMap<String, String>? = null
    ): InAppMessage? {
        // sort list with comparator
        val sortedInAppMessages = inAppMessages.sortedWith(InAppMessageComparator())

        // if screen name is empty, find in app message that has no screen name filter
        // if screen name is not empty, find in app message that screen name filter has screen name value
        // if screen name is not empty and could not found in app message with screen name filter, use in app message without screen name filter
        // Also control nextDisplayTime for showEveryXMinutes type in app messages
        val inAppMessageWithoutScreenName = sortedInAppMessages.firstOrNull { inAppMessage: InAppMessage ->
            inAppMessage.data.displayCondition.screenNameFilters.isNullOrEmpty() &&
                isDisplayTimeAvailable(inAppMessage) &&
                operateRealTimeValues(inAppMessage.data.displayCondition.displayRuleSet, params)
        }
        return if (screenName.isNullOrEmpty()) {
            inAppMessageWithoutScreenName
        } else {
            val inAppMessageWithScreenName = sortedInAppMessages.firstOrNull { inAppMessage: InAppMessage ->
                isDisplayTimeAvailable(inAppMessage) &&
                    inAppMessage.data.displayCondition.screenNameFilters?.firstOrNull { screenNameFilter ->
                        operateScreenValues(
                            screenNameFilter.value,
                            screenName,
                            screenNameFilter.operator
                        )
                    } != null &&
                    operateRealTimeValues(inAppMessage.data.displayCondition.displayRuleSet, params)
            }
            inAppMessageWithScreenName ?: inAppMessageWithoutScreenName
        }
    }

    private fun operateRealTimeValues(
        displayRuleSet: DisplayRuleSet?,
        params: HashMap<String, String>?
    ): Boolean {
        if (displayRuleSet != null) {
            when (displayRuleSet.logicOperator) {
                LogicOperator.AND.name -> {
                    return displayRuleSet.displayRules.all {
                        operateDisplayRule(it, params)
                    }
                }
                LogicOperator.OR.name -> {
                    return !displayRuleSet.displayRules.all {
                        !operateDisplayRule(it, params)
                    }
                }
            }
        }
        return true
    }

    private fun operateDisplayRule(
        displayRule: DisplayRule,
        params: HashMap<String, String>?
    ): Boolean {
        when (displayRule.logicOperator) {
            LogicOperator.AND.name -> {
                return displayRule.criterionList.all {
                    operateCriterion(it, params)
                }
            }
            LogicOperator.OR.name -> {
                return !displayRule.criterionList.all {
                    !operateCriterion(it, params)
                }
            }
        }
        return true
    }

    @SuppressLint("SimpleDateFormat")
    private fun operateCriterion(
        criterion: Criterion,
        params: HashMap<String, String>?
    ): Boolean {
        val subscription = Prefs.subscription
        return when (criterion.parameter) {
            SpecialRuleParameter.CATEGORY_PATH.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.categoryPath
                )
            }
            SpecialRuleParameter.CART_ITEM_COUNT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.cartItemCount
                )
            }
            SpecialRuleParameter.CART_AMOUNT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.cartAmount
                )
            }
            SpecialRuleParameter.STATE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.state
                )
            }
            SpecialRuleParameter.CITY.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.city
                )
            }
            SpecialRuleParameter.TIMEZONE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = subscription?.timezone
                )
            }
            SpecialRuleParameter.LANGUAGE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = subscription?.language
                )
            }
            SpecialRuleParameter.SCREEN_WIDTH.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Resources.getSystem().displayMetrics.widthPixels.toString()
                )
            }
            SpecialRuleParameter.SCREEN_HEIGHT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Resources.getSystem().displayMetrics.heightPixels.toString()
                )
            }
            SpecialRuleParameter.OS_VERSION.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Build.VERSION.SDK_INT.toString()
                )
            }
            SpecialRuleParameter.OS.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = "android"
                )
            }
            SpecialRuleParameter.DEVICE_NAME.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Build.DEVICE
                )
            }
            SpecialRuleParameter.COUNTRY.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = subscription?.country
                )
            }
            SpecialRuleParameter.MONTH.key -> {
                val dateFormat = SimpleDateFormat("MMMM")
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = dateFormat.format(Date())
                )
            }
            SpecialRuleParameter.WEEK_DAY.key -> {
                val dateFormat = SimpleDateFormat("EEEE")
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = dateFormat.format(Date())
                )
            }
            SpecialRuleParameter.HOUR.key -> {
                val dateFormat = SimpleDateFormat("HH")
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = dateFormat.format(Date())
                )
            }
            SpecialRuleParameter.PAGE_VIEW_IN_VISIT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.pageViewVisitCount.toString()
                )
            }
            SpecialRuleParameter.ANONYMOUS.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = subscription?.contactKey.isNullOrEmpty().toString()
                )
            }
            SpecialRuleParameter.VISIT_DURATION.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = (TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis()) - Prefs.lastSessionStartTime).toString()
                )
            }
            SpecialRuleParameter.FIRST_VISIT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Prefs.firstLaunchTime.toString()
                )
            }
            SpecialRuleParameter.LAST_VISIT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Prefs.lastSessionVisitTime.toString()
                )
            }
            SpecialRuleParameter.BRAND_NAME.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Build.BRAND
                )
            }
            SpecialRuleParameter.MODEL_NAME.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Build.MODEL
                )
            }
            else -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = params?.get(criterion.parameter)
                )
            }
        }
    }

    private fun operateRuleParameter(
        operator: String,
        dataType: String,
        ruleParam: List<String>?,
        userParam: String?
    ): Boolean {
        if (ruleParam.isNullOrEmpty() || userParam == null) return true
        when (operator) {
            Operator.EQUALS.operator -> {
                return ruleParam.firstOrNull { it.lowercase() == userParam.lowercase() } != null
            }
            Operator.NOT_EQUALS.operator -> {
                return ruleParam.firstOrNull { it.lowercase() == userParam.lowercase() } == null
            }
            Operator.LIKE.operator -> {
                return ruleParam.firstOrNull { userParam.lowercase().contains(it.lowercase()) } != null
            }
            Operator.NOT_LIKE.operator -> {
                return ruleParam.firstOrNull { userParam.lowercase().contains(it.lowercase()) } == null
            }
            Operator.STARTS_WITH.operator -> {
                return ruleParam.firstOrNull { userParam.lowercase().startsWith(it.lowercase()) } != null
            }
            Operator.NOT_STARTS_WITH.operator -> {
                return ruleParam.firstOrNull { userParam.lowercase().startsWith(it.lowercase()) } == null
            }
            Operator.ENDS_WITH.operator -> {
                return ruleParam.firstOrNull { userParam.lowercase().endsWith(it.lowercase()) } != null
            }
            Operator.NOT_ENDS_WITH.operator -> {
                return ruleParam.firstOrNull { userParam.lowercase().endsWith(it.lowercase()) } == null
            }
            Operator.IN.operator -> {
                return ruleParam.firstOrNull { it.lowercase() == userParam.lowercase() } != null
            }
            Operator.NOT_IN.operator -> {
                return ruleParam.firstOrNull { it.lowercase() == userParam.lowercase() } == null
            }
            Operator.GREATER_THAN.operator -> {
                when (dataType) {
                    DataType.INT.name, DataType.DATETIME.name -> {
                        try {
                            val convertedRuleParam = mutableListOf<Long>()
                            ruleParam.forEach {
                                if (it.isDigitsOnly()) {
                                    convertedRuleParam.add(it.toLong())
                                }
                            }

                            if (userParam.isDigitsOnly()) {
                                val convertedUserParam = userParam.toLong()
                                return convertedRuleParam.firstOrNull { convertedUserParam <= it } == null
                            }
                        } catch (e: Exception) {
                            return true
                        }
                    }
                    else -> {
                        return true
                    }
                }
            }
            Operator.GREATER_EQUAL.operator -> {
                when (dataType) {
                    DataType.INT.name, DataType.DATETIME.name -> {
                        try {
                            val convertedRuleParam = mutableListOf<Long>()
                            ruleParam.forEach {
                                if (it.isDigitsOnly()) {
                                    convertedRuleParam.add(it.toLong())
                                }
                            }

                            if (userParam.isDigitsOnly()) {
                                val convertedUserParam = userParam.toLong()
                                return convertedRuleParam.firstOrNull { convertedUserParam < it } == null
                            }
                        } catch (e: Exception) {
                            return true
                        }
                    }
                    else -> {
                        return true
                    }
                }
            }
            Operator.LESS_THAN.operator -> {
                when (dataType) {
                    DataType.INT.name, DataType.DATETIME.name -> {
                        try {
                            val convertedRuleParam = mutableListOf<Long>()
                            ruleParam.forEach {
                                if (it.isDigitsOnly()) {
                                    convertedRuleParam.add(it.toLong())
                                }
                            }

                            if (userParam.isDigitsOnly()) {
                                val convertedUserParam = userParam.toLong()
                                return convertedRuleParam.firstOrNull { convertedUserParam >= it } == null
                            }
                        } catch (e: Exception) {
                            return true
                        }
                    }
                    else -> {
                        return true
                    }
                }
            }
            Operator.LESS_EQUAL.operator -> {
                when (dataType) {
                    DataType.INT.name, DataType.DATETIME.name -> {
                        try {
                            val convertedRuleParam = mutableListOf<Long>()
                            ruleParam.forEach {
                                if (it.isDigitsOnly()) {
                                    convertedRuleParam.add(it.toLong())
                                }
                            }

                            if (userParam.isDigitsOnly()) {
                                val convertedUserParam = userParam.toLong()
                                return convertedRuleParam.firstOrNull { convertedUserParam > it } == null
                            }
                        } catch (e: Exception) {
                            return true
                        }
                    }
                    else -> {
                        return true
                    }
                }
            }
            Operator.BETWEEN.operator -> {
                when (dataType) {
                    DataType.INT.name, DataType.DATETIME.name -> {
                        try {
                            val convertedRuleParam = mutableListOf<Long>()
                            ruleParam.forEach {
                                if (it.isDigitsOnly()) {
                                    convertedRuleParam.add(it.toLong())
                                }
                            }

                            if (userParam.isDigitsOnly() && convertedRuleParam.size >= 2) {
                                val convertedUserParam = userParam.toLong()

                                val firstRuleParam = convertedRuleParam.first()
                                val lastRuleParam = convertedRuleParam.last()
                                return (convertedUserParam in (firstRuleParam + 1) until lastRuleParam) ||
                                    (convertedUserParam in (lastRuleParam + 1) until firstRuleParam)
                            }
                        } catch (e: Exception) {
                            return true
                        }
                    }
                    else -> {
                        return true
                    }
                }
            }
            Operator.NOT_BETWEEN.operator -> {
                when (dataType) {
                    DataType.INT.name, DataType.DATETIME.name -> {
                        try {
                            val convertedRuleParam = mutableListOf<Long>()
                            ruleParam.forEach {
                                if (it.isDigitsOnly()) {
                                    convertedRuleParam.add(it.toLong())
                                }
                            }

                            if (userParam.isDigitsOnly() && convertedRuleParam.size >= 2) {
                                val convertedUserParam = userParam.toLong()

                                val firstRuleParam = convertedRuleParam.first()
                                val lastRuleParam = convertedRuleParam.last()
                                return convertedUserParam !in (firstRuleParam + 1) until lastRuleParam &&
                                    convertedUserParam !in (lastRuleParam + 1) until firstRuleParam
                            }
                        } catch (e: Exception) {
                            return true
                        }
                    }
                    else -> {
                        return true
                    }
                }
            }
        }
        return true
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
