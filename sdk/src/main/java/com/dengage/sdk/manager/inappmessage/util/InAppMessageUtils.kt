package com.dengage.sdk.manager.inappmessage.util

import android.annotation.SuppressLint
import android.content.Context
import android.content.res.Resources
import android.os.Build
import android.util.TypedValue
import androidx.core.text.isDigitsOnly
import com.dengage.sdk.domain.inappmessage.DebugLogRequest
import com.dengage.sdk.domain.inappmessage.DebugLoggingRepository
import com.dengage.sdk.manager.session.SessionManager
import org.joda.time.DateTime
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.joda.time.Days
import java.text.ParseException
import java.text.SimpleDateFormat
import java.util.*
import java.util.concurrent.TimeUnit
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.*
import com.dengage.sdk.manager.visitcount.VisitCountManager
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.GsonHolder
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch

object InAppMessageUtils {

    private val debugLoggingRepository = DebugLoggingRepository()
    private val debugScope = CoroutineScope(Dispatchers.IO)

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
            } catch (e: Exception) {
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
        propertyId: String? = "",
        storyPropertyId: String? = null,
    ): InAppMessage? {
        val subscription = Prefs.subscription
        val sdkParameters = Prefs.sdkParameters
        val isDebugDevice = isDebugDevice(subscription?.getSafeDeviceId(), sdkParameters?.debugDeviceIds)
        val traceId = if (isDebugDevice) UUID.randomUUID().toString() else null
        val currentCampaignList = if (isDebugDevice) {
            inAppMessages.map { inAppMessage ->
                val prefix = if (inAppMessage.data.isRealTime()) "r_" else "f_"
                prefix + (inAppMessage.data.publicId ?: inAppMessage.id)
            }
        } else emptyList()

        // sort list with comparator
        val sortedInAppMessages = inAppMessages.sortedWith(InAppMessageComparator())

        // if screen name is empty, find in app message that has no screen name filter
        // if screen name is not empty, find in app message that screen name filter has screen name value
        // if screen name is not empty and could not found in app message with screen name filter, use in app message without screen name filter
        // Also control nextDisplayTime for showEveryXMinutes type in app messages
        val matchedWithoutScreenFilters =
            sortedInAppMessages.firstOrNull { inAppMessage: InAppMessage ->

                val context: MutableMap<String, String>? =
                    if (isDebugDevice) mutableMapOf<String, String>() else null
                var criterionIndex = 0

                // Check display time availability and add context
                val isDisplayTimeAvailable = if (isDebugDevice && context != null) {
                    inAppMessage.data.isDisplayTimeAvailable(context, criterionIndex).also {
                        criterionIndex += 2 // We add 2 entries for time constraints
                    }
                } else {
                    inAppMessage.data.isDisplayTimeAvailable()
                }

                val result = inAppMessage.data.displayCondition.screenNameFilters.isNullOrEmpty() &&
                        isInlineInApp(inAppMessage, propertyId, storyPropertyId) &&
                        isDisplayTimeAvailable &&
                        operateRealTimeValues(
                            inAppMessage.data.displayCondition.displayRuleSet,
                            params, context, criterionIndex
                        )

                if (isDebugDevice && traceId != null && inAppMessage.data.displayCondition.screenNameFilters.isNullOrEmpty() && context != null) {
                    sendEvaluationLog(
                        inAppMessage = inAppMessage,
                        traceId = traceId,
                        screenName = screenName,
                        currentCampaignList = currentCampaignList,
                        matched = result,
                        context = context
                    )
                }
                result
            }

        return if (screenName.isNullOrEmpty()) {
            matchedWithoutScreenFilters
        } else {
            val matchedWithScreenFilters = sortedInAppMessages.firstOrNull { inAppMessage ->
                val context: MutableMap<String, String>? =
                    if (isDebugDevice) mutableMapOf<String, String>() else null
                var criterionIndex = 0

                // Add screen name context if present
                val screenNameFilter = inAppMessage.data.displayCondition.screenNameFilters?.firstOrNull()
                val expectedScreenName = screenNameFilter?.value?.joinToString(",") ?: ""
                val operator = screenNameFilter?.operator ?: "EQUALS"
                val screenNameResult = isScreenNameFound(inAppMessage, screenName)
                if(context != null) {
                    context["screen_name_${criterionIndex}"] = "$expectedScreenName|$screenName|$operator|$screenNameResult"
                    criterionIndex++
                }
                // Check display time availability and add context
                val isDisplayTimeAvailable = if (isDebugDevice && context != null) {
                    inAppMessage.data.isDisplayTimeAvailable(context, criterionIndex).also {
                        criterionIndex += 2 // We add 2 entries for time constraints
                    }
                } else {
                    inAppMessage.data.isDisplayTimeAvailable()
                }

                val result = !inAppMessage.data.displayCondition.screenNameFilters.isNullOrEmpty() &&
                        isDisplayTimeAvailable &&
                        isScreenNameFound(inAppMessage, screenName) &&
                        isInlineInApp(inAppMessage, propertyId, storyPropertyId) &&
                        operateRealTimeValues(
                            inAppMessage.data.displayCondition.displayRuleSet,
                            params, context, criterionIndex
                        )

                if (isDebugDevice && traceId != null && context != null) {
                    sendEvaluationLog(
                        inAppMessage = inAppMessage,
                        traceId = traceId,
                        screenName = screenName,
                        currentCampaignList = currentCampaignList,
                        matched = result,
                        context = context
                    )
                }
                result
            }

            return matchedWithScreenFilters ?: matchedWithoutScreenFilters
        }

    }

    private fun isDebugDevice(deviceId: String?, debugDeviceIds: List<String>?): Boolean {
        return !deviceId.isNullOrEmpty() && !debugDeviceIds.isNullOrEmpty() && debugDeviceIds.contains(deviceId)
    }

    private fun isScreenNameFound(inAppMessage: InAppMessage, screenName: String): Boolean {
        val screenNameCheckArrayList = mutableListOf<Boolean>()
        val operatorFilter = inAppMessage.data.displayCondition.screenNameFilterLogicOperator

        inAppMessage.data.displayCondition.screenNameFilters?.forEach { screenNameFilter ->
            screenNameCheckArrayList.add(
                operateScreenValues(
                    screenNameFilter.value,
                    screenName,
                    screenNameFilter.operator
                )
            )
        }

        return when {
            operatorFilter.equals("AND") -> screenNameCheckArrayList.none { !it }
            operatorFilter.equals("OR") -> screenNameCheckArrayList.any { it }
            else -> useOldScreenNameFilter(inAppMessage, screenName)
        }
        /*     return inAppMessage.data.displayCondition.screenNameFilters?.firstOrNull { screenNameFilter ->
                  operateScreenValues(
                      screenNameFilter.value,
                      screenName,
                      screenNameFilter.operator
                  )
              } != null*/


    }


    private fun isInlineInApp(inAppMessage: InAppMessage, propertyId: String?, storyPropertyId: String?): Boolean {
        if ("STORY".equals(inAppMessage.data.content.type, ignoreCase = true)) {
            val isPropertyEmpty = storyPropertyId.isNullOrEmpty()
            val isSelectorEmpty = inAppMessage.data.inlineTarget?.androidSelector.isNullOrEmpty()
            return if (isPropertyEmpty || isSelectorEmpty) {
                false
            } else {
                inAppMessage.data.inlineTarget?.androidSelector == storyPropertyId
            }
        } else if ("INLINE".equals(inAppMessage.data.content.type, ignoreCase = true)) {
            val isPropertyEmpty = propertyId.isNullOrEmpty()
            val isSelectorEmpty = inAppMessage.data.inlineTarget?.androidSelector.isNullOrEmpty()
            return if (isPropertyEmpty || isSelectorEmpty) {
                false
            } else {
                inAppMessage.data.inlineTarget?.androidSelector == propertyId
            }
        } else if (storyPropertyId.isNullOrEmpty()) {
            return if (propertyId.isNullOrEmpty() && inAppMessage.data.inlineTarget?.androidSelector?.isNotEmpty() == true) {
                false
            } else if (inAppMessage.data.inlineTarget?.androidSelector?.isEmpty() == true && propertyId.isNullOrEmpty()) {
                true
            } else if (!propertyId.isNullOrEmpty() && inAppMessage.data.inlineTarget?.androidSelector?.isEmpty() == true) {
                false
            } else if (!propertyId.isNullOrEmpty() && inAppMessage.data.inlineTarget?.androidSelector?.isNotEmpty() == true) {
                inAppMessage.data.inlineTarget?.androidSelector?.equals(propertyId) == true
            } else if (!propertyId.isNullOrEmpty() && inAppMessage.data.inlineTarget?.androidSelector?.isEmpty() == null) {
                false
            } else true
        }
        return false
        //return true
    }

    private fun useOldScreenNameFilter(inAppMessage: InAppMessage, screenName: String): Boolean {
        return inAppMessage.data.displayCondition.screenNameFilters?.firstOrNull { screenNameFilter ->
            operateScreenValues(
                screenNameFilter.value,
                screenName,
                screenNameFilter.operator
            )
        } != null
    }

    private fun operateRealTimeValues(
        displayRuleSet: DisplayRuleSet?,
        params: HashMap<String, String>?,
        context: MutableMap<String, String>? = null,
        startingIndex: Int = 0
    ): Boolean {
        var criterionIndex = startingIndex
        if (displayRuleSet != null) {
            when (displayRuleSet.logicOperator) {
                LogicOperator.AND.name -> {
                    return displayRuleSet.displayRules.all {
                        operateDisplayRule(it, params, context, criterionIndex)
                    }
                }
                LogicOperator.OR.name -> {
                    return displayRuleSet.displayRules.any {
                        operateDisplayRule(it, params, context, criterionIndex)
                    }
                }
            }
        }
        return true
    }

    private fun operateDisplayRule(
        displayRule: DisplayRule,
        params: HashMap<String, String>?,
        context: MutableMap<String, String>? = null,
        startingIndex: Int = 0
    ): Boolean {
        var criterionIndex = startingIndex
        when (displayRule.logicOperator) {
            LogicOperator.AND.name -> {
                return displayRule.criterionList.all {
                    val result = operateCriterion(it, params, context, criterionIndex)
                    if (context != null) {
                        criterionIndex++
                    }
                    result
                }
            }
            LogicOperator.OR.name -> {
                return displayRule.criterionList.any {
                    val result = operateCriterion(it, params, context, criterionIndex)
                    if (context != null) {
                        criterionIndex++
                    }
                    result
                }
            }
        }
        return true
    }

    @SuppressLint("SimpleDateFormat")
    private fun operateCriterion(
        criterion: Criterion,
        params: HashMap<String, String>?,
        context: MutableMap<String, String>? = null,
        criterionIndex: Int = 0
    ): Boolean {
        val subscription = Prefs.subscription
        val visitorInfo = Prefs.visitorInfo

        // temporary return for empty parameter. Another library should be used instead of Gson
        if (criterion.parameter.isNullOrEmpty()) {
            return false
        }

        var actualValue = when (criterion.parameter) {
            SpecialRuleParameter.CATEGORY_PATH.key -> RealTimeInAppParamHolder.categoryPath ?: ""
            SpecialRuleParameter.CART_ITEM_COUNT.key -> RealTimeInAppParamHolder.cartItemCount ?: "0"
            SpecialRuleParameter.CART_AMOUNT.key -> RealTimeInAppParamHolder.cartAmount ?: "0"
            SpecialRuleParameter.STATE.key -> RealTimeInAppParamHolder.state ?: ""
            SpecialRuleParameter.CITY.key -> RealTimeInAppParamHolder.city ?: ""
            SpecialRuleParameter.TIMEZONE.key -> subscription?.timezone ?: ""
            SpecialRuleParameter.LANGUAGE.key -> subscription?.language ?: ""
            SpecialRuleParameter.SCREEN_WIDTH.key -> Resources.getSystem().displayMetrics.widthPixels.toString()
            SpecialRuleParameter.SCREEN_HEIGHT.key -> Resources.getSystem().displayMetrics.heightPixels.toString()
            SpecialRuleParameter.OS_VERSION.key -> Build.VERSION.RELEASE.toString()
            SpecialRuleParameter.OS.key -> "android"
            SpecialRuleParameter.DEVICE_NAME.key -> Build.DEVICE
            SpecialRuleParameter.COUNTRY.key -> subscription?.country ?: ""
            SpecialRuleParameter.MONTH.key -> {
                val dateFormat = SimpleDateFormat("MMM")
                dateFormat.format(Date())
            }
            SpecialRuleParameter.WEEK_DAY.key -> {
                val dateFormat = SimpleDateFormat("EEE")
                dateFormat.format(Date())
            }
            SpecialRuleParameter.HOUR.key -> {
                val dateFormat = SimpleDateFormat("HH")
                dateFormat.format(Date())
            }
            SpecialRuleParameter.PAGE_VIEW_IN_VISIT.key -> RealTimeInAppParamHolder.pageViewVisitCount.toString()
            SpecialRuleParameter.ANONYMOUS.key -> subscription?.contactKey.isNullOrEmpty().toString()
            SpecialRuleParameter.VISIT_DURATION.key -> {
                val visitDurationInMinutes = ((TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis()) - Prefs.lastSessionStartTime) / 60).toInt()
                visitDurationInMinutes.toString()
            }
            SpecialRuleParameter.FIRST_VISIT.key -> {
                if (System.currentTimeMillis() / 1000 - Prefs.firstLaunchTime < 3600) "true" else "false"
            }
            SpecialRuleParameter.LAST_VISIT.key -> Prefs.lastSessionVisitTime.toString()
            SpecialRuleParameter.BRAND_NAME.key -> Build.BRAND
            SpecialRuleParameter.MODEL_NAME.key -> Build.MODEL
            SpecialRuleParameter.PUSH_PERMISSION.key -> (subscription?.permission == true && subscription.token.isNullOrEmpty()
                .not()).toString()
            SpecialRuleParameter.LAST_PRODUCT_ID.key -> RealTimeInAppParamHolder.getLastProductId() ?: ""
            SpecialRuleParameter.LAST_PRODUCT_PRICE.key -> RealTimeInAppParamHolder.getLastProductPrice() ?: ""
            SpecialRuleParameter.LAST_CATEGORY_PATH.key -> RealTimeInAppParamHolder.getLastCategoryPath() ?: ""
            SpecialRuleParameter.CURRENT_PAGE_TITLE.key -> RealTimeInAppParamHolder.getCurrentPageTitle() ?: ""
            SpecialRuleParameter.CURRENT_PAGE_TYPE.key -> RealTimeInAppParamHolder.getCurrentPageType() ?: ""
            checkVisitorInfoAttr(criterion.parameter) -> {
                if (criterion.dataType == DataType.DATETIME.name) {
                    changeDateFormat(DateTime.now().toLocalDateTime().toString())
                } else {
                    getVisitorInfoAttrValue(criterion).toString()
                }
            }
            else -> {
                if (!criterion.parameter.contains("dn.")) {
                    params?.get(criterion.parameter) ?: ""
                } else {
                    getVisitorInfoAttrValue(criterion).toString()
                }
            }
        }

        val result = when (criterion.parameter) {
            SpecialRuleParameter.CATEGORY_PATH.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.CART_ITEM_COUNT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.CART_AMOUNT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.STATE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.CITY.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.TIMEZONE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.LANGUAGE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.SCREEN_WIDTH.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.SCREEN_HEIGHT.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.OS_VERSION.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.OS.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.DEVICE_NAME.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.COUNTRY.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
                )
            }
            SpecialRuleParameter.MONTH.key -> {
                val dateFormat = SimpleDateFormat("MMM")
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = dateFormat.format(Date())
                )
            }
            SpecialRuleParameter.WEEK_DAY.key -> {
                val dateFormat = SimpleDateFormat("EEE")
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
                val visitDurationInMinutes = ((TimeUnit.MILLISECONDS.toSeconds(System.currentTimeMillis()) - Prefs.lastSessionStartTime) / 60).toInt()
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = visitDurationInMinutes.toString()
                )
            }
            SpecialRuleParameter.FIRST_VISIT.key -> {

                var firstVisit = "false"
                if (System.currentTimeMillis() / 1000 - Prefs.firstLaunchTime < 3600) {
                    firstVisit = "true"
                }

                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = firstVisit
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
            SpecialRuleParameter.PUSH_PERMISSION.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = actualValue
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
                            actualValue = VisitCountManager.findVisitCountSinceDays(visitCount.timeAmount).toString()
                            operateRuleParameter(
                                operator = criterion.operator,
                                dataType = DataType.INT.name,
                                ruleParam = listOf(visitCount.count.toString()),
                                userParam = actualValue
                            )
                        }
                    } else {
                        true
                    }
                } catch (e: Exception) {
                    e.printStackTrace()
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
                    actualValue = segments?.joinToString(",") ?: ""
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
                    actualValue = tags?.joinToString(",") ?: ""
                    operateVisitorRuleParameter(
                        operator = criterion.operator,
                        dataType = criterion.dataType,
                        ruleParam = criterion.values,
                        userParam = tags
                    )
                }
            }
            SpecialRuleParameter.EVENT_HISTORY.key -> {
                EventHistoryUtils.operateEventHistoryFilter(criterion)
            }
            SpecialRuleParameter.CART_ITEMS.key -> {
                CartUtils.operateCartFilter(criterion)
            }
            SpecialRuleParameter.LAST_PRODUCT_ID.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.getLastProductId() ?: ""
                )
            }
            SpecialRuleParameter.LAST_PRODUCT_PRICE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.getLastProductPrice() ?: ""
                )
            }
            SpecialRuleParameter.LAST_CATEGORY_PATH.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.getLastCategoryPath() ?: ""
                )
            }
            SpecialRuleParameter.CURRENT_PAGE_TITLE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.getCurrentPageTitle() ?: ""
                )
            }
            SpecialRuleParameter.CURRENT_PAGE_TYPE.key -> {
                operateRuleParameter(
                    operator = criterion.operator,
                    dataType = criterion.dataType,
                    ruleParam = criterion.values,
                    userParam = RealTimeInAppParamHolder.getCurrentPageType() ?: ""
                )
            }

            checkVisitorInfoAttr(criterion.parameter) -> {
                if (criterion.parameter == SpecialRuleParameter.BIRTH_DATE.key) {
                    return birthdayCriteriaValid(
                        criterion.values,
                        actualValue
                    )
                } else if (criterion.dataType == DataType.DATETIME.name) {
                    operateRuleParameter(
                        operator = criterion.operator,
                        dataType = criterion.dataType,
                        ruleParam = criterion.values,
                        userParam = actualValue
                    )

                } else {
                    operateRuleParameter(
                        operator = criterion.operator,
                        dataType = criterion.dataType,
                        ruleParam = criterion.values,
                        userParam = actualValue
                    )
                }
            }

            else -> {
                if (!criterion.parameter.contains("dn.")) {
                    operateRuleParameter(
                        operator = criterion.operator,
                        dataType = criterion.dataType,
                        ruleParam = criterion.values,
                        userParam = actualValue
                    )
                } else {
                    false
                }
            }
        }

        // Add to context if provided
        if (context != null) {
            val key = "${criterion.parameter}_${criterionIndex}"
            val expectedValue = criterion.values?.joinToString(",") ?: ""
            context[key] = "$expectedValue|$actualValue|${criterion.operator}|$result"
        }

        return result
    }

    private fun sendEvaluationLog(
        inAppMessage: InAppMessage,
        traceId: String,
        screenName: String?,
        currentCampaignList: List<String>,
        matched: Boolean,
        context: Map<String, String>
    ) {
        debugScope.launch {
            try {
                val subscription = Prefs.subscription
                val sdkParameters = Prefs.sdkParameters

                val campaignId = inAppMessage.data.publicId ?: inAppMessage.id

                val debugLog = DebugLogRequest(
                    traceId = traceId,
                    appGuid = sdkParameters?.appId,
                    appId = sdkParameters?.appId,
                    account = sdkParameters?.accountName,
                    device = subscription?.getSafeDeviceId() ?: "",
                    sessionId = SessionManager.getSessionId(),
                    sdkVersion = DengageUtils.getSdkVersion(),
                    currentCampaignList = currentCampaignList,
                    campaignId = campaignId,
                    campaignType = if (inAppMessage.data.isRealTime()) "realtime" else "bulk",
                    sendId = if (!inAppMessage.data.isRealTime()) inAppMessage.id else null,
                    message = if (matched) "Campaign matched for evaluation traceId:$traceId campaignId:$campaignId" else
                        "Campaign unmatched for evaluation traceId:$traceId campaignId:$campaignId",
                    context = context,
                    contactKey = subscription?.contactKey,
                    channel = "android",
                    currentRules = mapOf(
                        "displayCondition" to GsonHolder.toJson(inAppMessage.data.displayCondition)
                    )
                )

                debugLoggingRepository.sendDebugLog(screenName ?: "unknown", debugLog)
            } catch (e: Exception) {
                DengageLogger.error("Error sending evaluation debug log: ${e.message}")
            }
        }
    }

    private fun birthdayCriteriaValid(values: List<String>?, birthDateVisitorInfo: String?): Boolean {
        if (values.isNullOrEmpty() || birthDateVisitorInfo.isNullOrEmpty()) {
            return false
        }
        val comparisonValue = values[0].toIntOrNull() ?: return false

        val birthLocalDate = try {
            val fmt = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm:ss")
            fmt.parseLocalDate(birthDateVisitorInfo)
        } catch (e: Exception) {
            e.printStackTrace()
            return false
        }

        val birthMonth = birthLocalDate.monthOfYear
        val birthDay = birthLocalDate.dayOfMonth

        val today = LocalDate.now()
        val thisYearBirthday = birthLocalDate.withYear(today.year)

        return when {
            comparisonValue < 0 -> {
                val window = -comparisonValue

                val lastBirthday =
                    if (thisYearBirthday.isAfter(today))
                        thisYearBirthday.minusYears(1)
                    else
                        thisYearBirthday

                val daysSince = Days.daysBetween(lastBirthday, today).days
                daysSince in 0..window
            }

            comparisonValue == 0 -> {
                today.monthOfYear == birthMonth && today.dayOfMonth == birthDay
            }

            else -> {
                val window = comparisonValue
                val nextBirthday =
                    if (thisYearBirthday.isBefore(today))
                        thisYearBirthday.plusYears(1)
                    else
                        thisYearBirthday

                val daysUntil = Days.daysBetween(today, nextBirthday).days
                daysUntil in 0..window
            }
        }
    }

    private fun checkVisitorInfoAttr(parameter: String): String? {
        try {
            val attr: HashMap<String, String>? = Prefs.visitorInfo?.attr
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
            val attr: HashMap<String, String>? = Prefs.visitorInfo?.attr

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
        userParam: String?): Boolean {
        if (ruleParam.isNullOrEmpty() || userParam == null) return false
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
            Operator.CONTAINS_ALL.operator -> {
                return ruleParam.all { value ->
                    userParam.lowercase().contains(value.lowercase())
                }
            }
            Operator.CONTAINS_ANY.operator -> {
                return ruleParam.any { value ->
                    userParam.lowercase().contains(value.lowercase())
                }
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
                            e.printStackTrace()
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
                            e.printStackTrace()
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
                            e.printStackTrace()
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
                            e.printStackTrace()
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
                            e.printStackTrace()
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
                            e.printStackTrace()
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

    private fun operateScreenValues(
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
        dateString: String,
    ): String {
        val currentFormat = "yyyy-MM-dd'T'HH:mm:ss.SSS"
        val requiredFormat = "yyyy-MM-dd HH:mm:ss"
        var result = ""
        if (dateString.isEmpty()) {
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

    fun hexToPercentageOpacity(hex: String?): Double {
        // Ensure the hex string has correct length
        if (hex?.length != 2) {
            throw IllegalArgumentException("Hex string must be 2 characters long")
        }

        // Convert hex to decimal
        val decimal = hex.toInt(16)

        // Convert decimal to percentage and return
        return (decimal / 255.0) * 100
    }
}