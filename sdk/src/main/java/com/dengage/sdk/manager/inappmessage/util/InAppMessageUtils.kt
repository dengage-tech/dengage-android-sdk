package com.dengage.sdk.manager.inappmessage.util

import android.annotation.SuppressLint
import android.content.Context
import android.content.res.Resources
import android.os.Build
import android.util.TypedValue
import androidx.core.text.isDigitsOnly
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.data.cache.Prefs.visitorInfo
import com.dengage.sdk.domain.inappmessage.model.*
import com.dengage.sdk.manager.visitcount.VisitCountManager
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.GsonHolder
import org.joda.time.DateTime
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
        inAppMessages: List<InAppMessage>?,
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
        params: HashMap<String, String>? = null,
        isRealTime: Boolean = false
    ): InAppMessage? {
        // sort list with comparator
        val sortedInAppMessages = inAppMessages.sortedWith(InAppMessageComparator())

        // if screen name is empty, find in app message that has no screen name filter
        // if screen name is not empty, find in app message that screen name filter has screen name value
        // if screen name is not empty and could not found in app message with screen name filter, use in app message without screen name filter
        // Also control nextDisplayTime for showEveryXMinutes type in app messages
        val inAppMessageWithoutScreenName =
            sortedInAppMessages.firstOrNull { inAppMessage: InAppMessage ->
                inAppMessage.data.displayCondition.screenNameFilters.isNullOrEmpty() &&
                        inAppMessage.data.isDisplayTimeAvailable() &&
                        operateRealTimeValues(inAppMessage.data.displayCondition.displayRuleSet,
                            params,isRealTime)
            }
        return if (screenName.isNullOrEmpty()) {
            inAppMessageWithoutScreenName
        } else {
          return sortedInAppMessages.firstOrNull { inAppMessage: InAppMessage ->
                    inAppMessage.data.isDisplayTimeAvailable() &&
                            inAppMessage.data.displayCondition.screenNameFilters?.firstOrNull { screenNameFilter ->
                                operateScreenValues(
                                    screenNameFilter.value,
                                    screenName,
                                    screenNameFilter.operator
                                )
                            } != null &&
                            operateRealTimeValues(inAppMessage.data.displayCondition.displayRuleSet,
                                params,isRealTime)
                }

        }
    }

    private fun operateRealTimeValues(
        displayRuleSet: DisplayRuleSet?,
        params: HashMap<String, String>?,
        isRealTime: Boolean
    ): Boolean {
        if (displayRuleSet != null) {
            when (displayRuleSet.logicOperator) {
                LogicOperator.AND.name -> {
                    return displayRuleSet.displayRules.all {
                        operateDisplayRule(it, params, isRealTime)
                    }
                }
                LogicOperator.OR.name -> {
                    return !displayRuleSet.displayRules.all {
                        !operateDisplayRule(it, params, isRealTime)
                    }
                }
            }
        }
        return true
    }

    private fun operateDisplayRule(
        displayRule: DisplayRule,
        params: HashMap<String, String>?,
        isRealTime: Boolean,
    ): Boolean {
        when (displayRule.logicOperator) {
            LogicOperator.AND.name -> {
                return displayRule.criterionList.all {
                    operateCriterion(it, params, isRealTime)
                }
            }
            LogicOperator.OR.name -> {
                return !displayRule.criterionList.all {
                    !operateCriterion(it, params, isRealTime)
                }
            }
        }
        return true
    }

    @SuppressLint("SimpleDateFormat")
    private fun operateCriterion(
        criterion: Criterion,
        params: HashMap<String, String>?,
        isRealTime: Boolean,
    ): Boolean {
        val subscription = Prefs.subscription
        val visitorInfo = Prefs.visitorInfo

        return when (criterion.parameter) {
            SpecialRuleParameter.CATEGORY_PATH.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.categoryPath ?: "",
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.CART_ITEM_COUNT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.cartItemCount ?: "0",
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.CART_AMOUNT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.cartAmount ?: "0",
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.STATE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.state ?: "",
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.CITY.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.city ?: "",
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.TIMEZONE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = subscription?.timezone,
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.LANGUAGE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = subscription?.language,
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.SCREEN_WIDTH.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Resources.getSystem().displayMetrics.widthPixels.toString(),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.SCREEN_HEIGHT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Resources.getSystem().displayMetrics.heightPixels.toString(),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.OS_VERSION.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Build.VERSION.SDK_INT.toString(),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.OS.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = "android",
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.DEVICE_NAME.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Build.DEVICE,
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.COUNTRY.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = subscription?.country,
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.MONTH.key -> {
                val dateFormat = SimpleDateFormat("MM")
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = dateFormat.format(Date()),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.WEEK_DAY.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Calendar.getInstance().get(Calendar.DAY_OF_WEEK).toString(),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.HOUR.key -> {
                val dateFormat = SimpleDateFormat("HH")
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = dateFormat.format(Date()),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.PAGE_VIEW_IN_VISIT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.pageViewVisitCount.toString(),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.ANONYMOUS.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = subscription?.contactKey.isNullOrEmpty().toString(),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.VISIT_DURATION.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = (TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis()) - Prefs.lastSessionStartTime).toString(),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.FIRST_VISIT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Prefs.firstLaunchTime.toString(),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.LAST_VISIT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Prefs.lastSessionVisitTime.toString(),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.BRAND_NAME.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Build.BRAND,
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.MODEL_NAME.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = Build.MODEL,
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.PUSH_PERMISSION.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = (subscription?.permission ?: true).toString(),
                    isRealTime = isRealTime
                )
            }
            SpecialRuleParameter.VISIT_COUNT.key -> {
                try {
                    if (criterion.dataType == DataType.VISIT_COUNT_PAST_X_DAYS.name &&
                        criterion.values.isNullOrEmpty().not()
                    ) {
                        val visitCount = GsonHolder.fromJson<VisitCount>(criterion.values?.first())
                        if (visitCount == null) {
                            true
                        } else {
                            operateRuleParameter(
                                operator = criterion.operator,
                                dataType = DataType.INT.name,
                                ruleParam = listOf(visitCount.count.toString()),
                                userParam = VisitCountManager.findVisitCountSinceDays(visitCount.timeAmount)
                                    .toString(),
                                isRealTime = isRealTime
                            )
                        }
                    } else {
                        true
                    }
                } catch (e: Exception) {
                    true
                }
            }
            SpecialRuleParameter.SEGMENT.key -> {
                if (visitorInfo?.segments.isNullOrEmpty()) {
                    false
                } else {
                    val segments = visitorInfo?.segments?.map {
                        it.toString()
                    }
                    operateVisitorRuleParameter(
                        operator = criterion.operator,
                        dataType = criterion.dataType,
                        ruleParam = criterion.values,
                        userParam = segments
                    )
                }
            }
            SpecialRuleParameter.TAG.key -> {
                if (visitorInfo?.tags.isNullOrEmpty()) {
                    false
                } else {
                    val tags = visitorInfo?.tags?.map {
                        it.toString()
                    }
                    operateVisitorRuleParameter(
                        operator = criterion.operator,
                        dataType = criterion.dataType,
                        ruleParam = criterion.values,
                        userParam = tags
                    )
                }
            }

            checkVisitorInfoAttr(criterion.parameter) -> {
                if (criterion.parameter == SpecialRuleParameter.BIRTH_DATE.key) {
                    return birthCriteriaValid(criterion.values, getVisitorInfoAttrValue(criterion))
                }
                else if (criterion.dataType== DataType.DATETIME.name)
                {
                    operateRuleParameter(
                        operator = criterion.operator,
                        dataType = criterion.dataType,
                        ruleParam = criterion.values,
                        userParam = changeDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS","yyyy-MM-dd HH:mm:ss",DateTime.now().toLocalDateTime().toString()),
                        isRealTime = isRealTime
                    )

                }
                else {
                    operateRuleParameter(
                        operator = criterion.operator,
                        dataType = criterion.dataType,
                        ruleParam = criterion.values,
                        userParam = getVisitorInfoAttrValue(criterion).toString(),
                        isRealTime = isRealTime
                    )
                }
            }

            else -> {
                if (!criterion.parameter.contains("dn.")) {
                    operateRuleParameter(
                        operator = criterion.operator,
                        dataType = criterion.dataType,
                        ruleParam = criterion.values,
                        userParam = params?.get(criterion.parameter),
                        isRealTime = isRealTime
                    )
                } else {
                    return false
                }
            }

        }
    }


    private fun birthCriteriaValid(values: List<String>?, birthDateVisitoInfo: String?): Boolean {
        try {
            if (values.isNullOrEmpty()) return false
            val birthComparisonValue = values[0]
            if (birthComparisonValue.contains("-")) {
                val days =birthComparisonValue.replace("-", "").toInt()
                for (i in 0..days)
                {
                    if (DateTime.now().minusDays(days).plusDays(i).toString()
                            .split("T")[0] == (birthDateVisitoInfo?.split(" ")?.get(0) ?: "")
                    ) {
                        return true
                    }
                }

            } else if (birthComparisonValue == "0") {
                val birthDateBasedOnComparison = DateTime.now().toLocalDateTime()
                if (birthDateBasedOnComparison.toString()
                        .split("T")[0] == (birthDateVisitoInfo?.split(" ")?.get(0) ?: "")
                ) {
                    return true
                }
            } else {
                val days =birthComparisonValue.replace("-", "").toInt()
                for (i in 0..days)
                {
                    if (DateTime.now().plusDays(days).minusDays(i).toString()
                            .split("T")[0] == (birthDateVisitoInfo?.split(" ")?.get(0) ?: "")
                    ) {
                        return true
                    }
                }


            }
        } catch (_: Exception) {
        } catch (_: Throwable) {
        }
        return false

    }

    private fun checkVisitorInfoAttr(parameter: String): String? {
        try {
            val attr: HashMap<String, String>? = visitorInfo?.attr
            if (attr?.contains(parameter) == true) {
                return parameter
            }
        } catch (_: Exception) {
        } catch (_: Throwable) {
        }

        /* if(parameter == "dn.master_contact.birth_date") {
             return "dn.master_contact.birth_date"
         }*/
        return ""
    }

    private fun getVisitorInfoAttrValue(parameter: Criterion): String? {
        try {
            val attr: HashMap<String, String>? = visitorInfo?.attr

            if (attr?.contains(parameter.parameter) == true) {
                return attr[parameter.parameter]
            }

        } catch (_: Exception) {
        } catch (_: Throwable) {
        }

        /*if(parameter.parameter == "dn.master_contact.birth_date") {
            return "2023-05-29 04:04:09"
        }*/
        return ""
    }

    private fun operateVisitorRuleParameter(
        operator: String,
        dataType: String,
        ruleParam: List<String>?,
        userParam: List<String>?,
    ): Boolean {
        // visitor rules only work with IN and NOT_IN operator
        if (ruleParam != null && userParam != null && dataType == DataType.TEXTLIST.name) {
            val ruleContainsUserParam = userParam.firstOrNull {
                ruleParam.contains(it)
            } != null
            when (operator) {
                Operator.IN.operator -> {
                    return ruleContainsUserParam
                }
                Operator.NOT_IN.operator -> {
                    return !ruleContainsUserParam
                }
            }
        }
        return true
    }

    private fun operateRuleParameter(
        operator: String,
        dataType: String,
        ruleParam: List<String>?,
        userParam: String?,
        isRealTime: Boolean,
    ): Boolean {
        if (ruleParam.isNullOrEmpty() || userParam == null) return !isRealTime
        when (operator) {
            Operator.EQUALS.operator -> {
                return ruleParam.firstOrNull { it.lowercase() == userParam.lowercase() } != null
            }
            Operator.NOT_EQUALS.operator -> {
                return ruleParam.firstOrNull { it.lowercase() == userParam.lowercase() } == null
            }
            Operator.LIKE.operator -> {
                return ruleParam.firstOrNull {
                    userParam.lowercase().contains(it.lowercase())
                } != null
            }
            Operator.NOT_LIKE.operator -> {
                return ruleParam.firstOrNull {
                    userParam.lowercase().contains(it.lowercase())
                } == null
            }
            Operator.STARTS_WITH.operator -> {
                return ruleParam.firstOrNull {
                    userParam.lowercase().startsWith(it.lowercase())
                } != null
            }
            Operator.NOT_STARTS_WITH.operator -> {
                return ruleParam.firstOrNull {
                    userParam.lowercase().startsWith(it.lowercase())
                } == null
            }
            Operator.ENDS_WITH.operator -> {
                return ruleParam.firstOrNull {
                    userParam.lowercase().endsWith(it.lowercase())
                } != null
            }
            Operator.NOT_ENDS_WITH.operator -> {
                return ruleParam.firstOrNull {
                    userParam.lowercase().endsWith(it.lowercase())
                } == null
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
        screenNameFilterValue: List<String>?,
        screenName: String,
        operator: String,
    ): Boolean {
        val screenNameFilterValueSafe = screenNameFilterValue?.firstOrNull() ?: ""
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
                return screenNameFilterValue?.contains(screenName) ?: false
            }
            Operator.NOT_IN.operator -> {
                return !(screenNameFilterValue?.contains(screenName) ?: true)
            }
        }
        return true
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

    private fun changeDateFormat(
        currentFormat: String,
        requiredFormat: String,
        dateString: String,
    ): String {
        var result = ""
        if (dateString.isNullOrEmpty()) {
            return result
        }
        val formatterOld = SimpleDateFormat(currentFormat, Locale.getDefault())
        val formatterNew = SimpleDateFormat(requiredFormat, Locale.getDefault())
        var date: Date? = null
        try {
            date = formatterOld.parse(dateString)
        } catch (e: ParseException) {
            e.printStackTrace()
        }
        if (date != null) {
            result = formatterNew.format(date)
        }
        return result
    }

}