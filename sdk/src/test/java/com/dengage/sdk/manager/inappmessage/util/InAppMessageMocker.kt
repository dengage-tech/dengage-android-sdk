package com.dengage.sdk.manager.inappmessage.util

import com.dengage.sdk.domain.inappmessage.model.*

object InAppMessageMocker {

    fun createInAppMessage(
        id: String,
        priority: Priority,
        expireDate: String,
        screenName: String? = null,
        operator: Operator? = null,
        isRealTime: Boolean = false,
        hasRules: Boolean = true,
        maxShowCount: Int = 1,
        showCount: Long = 0,
        androidSelector: String = ""
    ): InAppMessage {
        val contentParams = ContentParams(
            position = ContentPosition.BOTTOM.position,
            shouldAnimate = true,
            html = null,
            maxWidth = null,
            radius = null,
            marginTop = null,
            marginBottom = null,
            marginLeft = null,
            marginRight = null,
            dismissOnTouchOutside = false,
            backgroundColor = "#FFFFFF",
            storySet = null
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )

        val displayCondition = DisplayCondition(
            screenNameFilters = if (screenName != null && operator != null) {
                listOf(
                    ScreenNameFilter(
                        value = listOf(screenName),
                        operator = operator.operator
                    )
                )
            } else {
                null
            },
            displayRuleSet = if (isRealTime) {
                DisplayRuleSet(
                    logicOperator = LogicOperator.AND.name,
                    displayRules = if (hasRules) {
                        listOf(
                            DisplayRule(
                                logicOperator = LogicOperator.AND.name,
                                criterionList = listOf()
                            )
                        )
                    } else {
                        listOf()
                    }
                )
            } else {
                null
            }
        )
        val displayTiming = DisplayTiming(
            delay = 10,
            showEveryXMinutes = 5,
            maxShowCount = maxShowCount,
            maxDismissCount = 5
        )
        val inAppMessageData = InAppMessageData(
            messageDetails = "messageDetails",
            expireDate = expireDate,
            priority = priority.priority,
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming,
            publicId = if (isRealTime) {
                "publicId"
            } else {
                null
            },
            inlineTarget = InlineTarget(
                androidSelector = androidSelector,
            ),
            nextDisplayTime = 0,
            showCount = showCount,
            dismissCount = 2
        )

        return InAppMessage(
            id = id,
            data = inAppMessageData
        )
    }

    fun createRealTimeInAppMessage(
        id: String, priority: Priority,
        expireDate: String,
        screenName: String? = null,
        operator: Operator? = null,
        criterionList: MutableList<Criterion>? = null,
        criterionOperator: LogicOperator? = null,
        ruleSetOperator: LogicOperator? = null,
        maxShowCount: Int = 1,
        showCount: Long = 0,
        androidSelector: String = ""
    ): InAppMessage {
        val ruleSet = if (criterionList == null) {
            null
        } else {
            val displayRules = mutableListOf<DisplayRule>()
            displayRules.add(
                DisplayRule(
                    logicOperator = criterionOperator?.name ?: LogicOperator.AND.name,
                    criterionList = criterionList
                )
            )
            DisplayRuleSet(
                logicOperator = ruleSetOperator?.name ?: LogicOperator.AND.name,
                displayRules = displayRules
            )
        }

        val contentParams = ContentParams(
            position = ContentPosition.BOTTOM.position,
            shouldAnimate = true,
            html = null,
            maxWidth = null,
            radius = null,
            marginTop = null,
            marginBottom = null,
            marginLeft = null,
            marginRight = null,
            dismissOnTouchOutside = false,
            backgroundColor = "#FFFFFF",
            storySet = null
        )
        val content = Content(
            type = ContentType.HTML.type,
            targetUrl = null,
            params = contentParams
        )

        val displayCondition = DisplayCondition(
            screenNameFilters = if (screenName != null && operator != null) listOf(
                ScreenNameFilter(
                    value = listOf(screenName),
                    operator = operator.operator
                )
            )
            else null,
            displayRuleSet = ruleSet
        )
        val displayTiming = DisplayTiming(
            delay = 10,
            showEveryXMinutes = 5,
            maxShowCount = maxShowCount,
            maxDismissCount = 5
        )
        val inAppMessageData = InAppMessageData(
            messageDetails = "messageDetails",
            expireDate = expireDate,
            priority = priority.priority,
            content = content,
            displayCondition = displayCondition,
            displayTiming = displayTiming,
            publicId = "publicId",
            inlineTarget = InlineTarget(
                androidSelector = androidSelector,
            ),
            nextDisplayTime = 0,
            showCount = showCount,
            dismissCount = 2
        )

        return InAppMessage(
            id = id,
            data = inAppMessageData
        )
    }

}