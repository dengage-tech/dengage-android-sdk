package com.dengage.sdk.manager.inappmessage.util

import com.dengage.sdk.domain.inappmessage.model.Criterion
import com.dengage.sdk.domain.inappmessage.model.DataType
import com.dengage.sdk.domain.inappmessage.model.Operator
import com.dengage.sdk.domain.inappmessage.model.Priority
import com.dengage.sdk.domain.inappmessage.model.SpecialRuleParameter
import com.dengage.sdk.util.Constants
import org.junit.Assert
import org.junit.Test
import java.text.SimpleDateFormat
import java.util.*

class InAppMessageUtilsTest {

    @Test
    fun `findNotExpiredInAppMessages test`() {
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val calendar = Calendar.getInstance()
        calendar.add(Calendar.DAY_OF_YEAR, -2)
        val inAppMessageExpired = InAppMessageMocker.createInAppMessage(
            id = Math.random().toString(),
            priority = Priority.HIGH,
            expireDate = expireDateFormat.format(calendar.time)
        )
        calendar.add(Calendar.DAY_OF_YEAR, 4)
        val inAppMessageNotExpired = InAppMessageMocker.createInAppMessage(
            id = Math.random().toString(),
            priority = Priority.HIGH,
            expireDate = expireDateFormat.format(calendar.time)
        )

        val inAppMessages = listOf(inAppMessageExpired, inAppMessageNotExpired)
        val notExpiredInAppMessages = InAppMessageUtils.findNotExpiredInAppMessages(
            untilDate = Calendar.getInstance().time,
            inAppMessages = inAppMessages
        )
        Assert.assertEquals(notExpiredInAppMessages?.size, 1)
    }

    @Test
    fun `findPriorInAppMessage priority test`() {
        val id1 = Math.random().toString()
        val id2 = Math.random().toString()
        val id3 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val inAppMessagePriorityLow = InAppMessageMocker.createInAppMessage(
            id = id3,
            priority = Priority.LOW,
            expireDate = expireDate
        )
        val inAppMessagePriorityMedium = InAppMessageMocker.createInAppMessage(
            id = id2,
            priority = Priority.MEDIUM,
            expireDate = expireDate
        )
        val inAppMessagePriorityHigh = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate
        )

        val inAppMessages = listOf(inAppMessagePriorityLow, inAppMessagePriorityHigh, inAppMessagePriorityMedium)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `findPriorInAppMessage empty screen name test`() {
        val id1 = Math.random().toString()
        val id2 = Math.random().toString()
        val id3 = Math.random().toString()
        val id4 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val inAppMessage1 = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "screenName",
            operator = Operator.EQUALS
        )
        val inAppMessage2 = InAppMessageMocker.createInAppMessage(
            id = id2,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "screenName",
            operator = Operator.EQUALS
        )
        val inAppMessage3 = InAppMessageMocker.createInAppMessage(
            id = id3,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "screenName",
            operator = Operator.EQUALS
        )
        val inAppMessage4 = InAppMessageMocker.createInAppMessage(
            id = id4,
            priority = Priority.LOW,
            expireDate = expireDate
        )

        val inAppMessages = listOf(inAppMessage1, inAppMessage2,
            inAppMessage3, inAppMessage4)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages
        )
        Assert.assertEquals(priorInAppMessage?.id, id4)
    }

    @Test
    fun `findPriorInAppMessage screen name test`() {
        val id1 = Math.random().toString()
        val id2 = Math.random().toString()
        val id3 = Math.random().toString()
        val id4 = Math.random().toString()
        val id5 = Math.random().toString()
        val id6 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val inAppMessagePriorityLow1 = InAppMessageMocker.createInAppMessage(
            id = id6,
            priority = Priority.LOW,
            expireDate = expireDate,
            screenName = "screen1",
            operator = Operator.EQUALS
        )
        val inAppMessagePriorityLow2 = InAppMessageMocker.createInAppMessage(
            id = id5,
            priority = Priority.LOW,
            expireDate = expireDate,
            screenName = "screen2",
            operator = Operator.EQUALS
        )
        val inAppMessagePriorityMedium1 = InAppMessageMocker.createInAppMessage(
            id = id4,
            priority = Priority.MEDIUM,
            expireDate = expireDate,
            screenName = "screen1",
            operator = Operator.EQUALS
        )
        val inAppMessagePriorityMedium2 = InAppMessageMocker.createInAppMessage(
            id = id3,
            priority = Priority.MEDIUM,
            expireDate = expireDate,
            screenName = "screen3",
            operator = Operator.EQUALS
        )
        val inAppMessagePriorityHigh1 = InAppMessageMocker.createInAppMessage(
            id = id2,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "screen1",
            operator = Operator.EQUALS
        )
        val inAppMessagePriorityHigh2 = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "screen3",
            operator = Operator.EQUALS
        )

        val inAppMessages = listOf(inAppMessagePriorityMedium1, inAppMessagePriorityMedium2,
            inAppMessagePriorityHigh1, inAppMessagePriorityHigh2,
            inAppMessagePriorityLow2, inAppMessagePriorityLow1)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "screen2"
        )
        Assert.assertEquals(priorInAppMessage?.id, id5)
    }

    @Test
    fun `findPriorInAppMessage with screen name but has no screen name filter test`() {
        val id1 = Math.random().toString()
        val id2 = Math.random().toString()
        val id3 = Math.random().toString()
        val id4 = Math.random().toString()
        val id5 = Math.random().toString()
        val id6 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val inAppMessagePriorityLow1 = InAppMessageMocker.createInAppMessage(
            id = id6,
            priority = Priority.LOW,
            expireDate = expireDate,
            screenName = "screen1",
            operator = Operator.EQUALS
        )
        val inAppMessagePriorityLow2 = InAppMessageMocker.createInAppMessage(
            id = id5,
            priority = Priority.LOW,
            expireDate = expireDate,
            screenName = "screen2",
            operator = Operator.EQUALS
        )
        val inAppMessagePriorityMedium1 = InAppMessageMocker.createInAppMessage(
            id = id4,
            priority = Priority.MEDIUM,
            expireDate = expireDate,
            screenName = "screen1",
            operator = Operator.EQUALS
        )
        val inAppMessagePriorityMedium2 = InAppMessageMocker.createInAppMessage(
            id = id3,
            priority = Priority.MEDIUM,
            expireDate = expireDate
        )
        val inAppMessagePriorityHigh1 = InAppMessageMocker.createInAppMessage(
            id = id2,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "screen1",
            operator = Operator.EQUALS
        )
        val inAppMessagePriorityHigh2 = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate
        )

        val inAppMessages = listOf(inAppMessagePriorityMedium1, inAppMessagePriorityMedium2,
            inAppMessagePriorityHigh1, inAppMessagePriorityHigh2,
            inAppMessagePriorityLow2, inAppMessagePriorityLow1)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "screen3"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `findPriorInAppMessage expire date test`() {
        val id1 = Math.random().toString()
        val id2 = Math.random().toString()
        val id3 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val calendar = Calendar.getInstance()
        calendar.add(Calendar.DAY_OF_YEAR, 1)
        val inAppMessagePriorityLow1 = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.LOW,
            expireDate = expireDateFormat.format(calendar.time)
        )
        calendar.add(Calendar.DAY_OF_YEAR, 2)
        val inAppMessagePriorityLow2 = InAppMessageMocker.createInAppMessage(
            id = id2,
            priority = Priority.LOW,
            expireDate = expireDateFormat.format(calendar.time)
        )
        calendar.add(Calendar.DAY_OF_YEAR, 3)
        val inAppMessagePriorityLow3 = InAppMessageMocker.createInAppMessage(
            id = id3,
            priority = Priority.LOW,
            expireDate = expireDateFormat.format(calendar.time)
        )

        val inAppMessages = listOf(inAppMessagePriorityLow2, inAppMessagePriorityLow1, inAppMessagePriorityLow3)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `findPriorInAppMessage display timing test`() {
        val id1 = Math.random().toString()
        val id2 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val inAppMessageIsAvailableTiming = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.MEDIUM,
            expireDate = expireDate
        )
        val inAppMessageIsNotAvailableTiming = InAppMessageMocker.createInAppMessage(
            id = id2,
            priority = Priority.HIGH,
            expireDate = expireDate
        )
        inAppMessageIsAvailableTiming.data.nextDisplayTime = System.currentTimeMillis() - 1000
        inAppMessageIsNotAvailableTiming.data.nextDisplayTime = System.currentTimeMillis() + 10000

        val inAppMessages = listOf(inAppMessageIsNotAvailableTiming, inAppMessageIsAvailableTiming)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `findPriorInAppMessage real time test`() {
        val id1 = Math.random().toString()
        val id2 = Math.random().toString()
        val id3 = Math.random().toString()
        val id4 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val inAppMessageRealTimeLow = InAppMessageMocker.createInAppMessage(
            id = id4,
            priority = Priority.LOW,
            expireDate = expireDate,
            isRealTime = true
        )
        val inAppMessageNotRealTimeLow = InAppMessageMocker.createInAppMessage(
            id = id2,
            priority = Priority.LOW,
            expireDate = expireDate,
            isRealTime = false
        )
        val inAppMessageRealTimeHigh = InAppMessageMocker.createInAppMessage(
            id = id3,
            priority = Priority.HIGH,
            expireDate = expireDate,
            isRealTime = true
        )
        val inAppMessageNotRealTimeHigh = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            isRealTime = false
        )

        val inAppMessages = listOf(inAppMessageRealTimeLow, inAppMessageRealTimeHigh, inAppMessageNotRealTimeLow, inAppMessageNotRealTimeHigh)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `In App Comparator real time test`() {
        val id1 = Math.random().toString()
        val id2 = Math.random().toString()
        val id3 = Math.random().toString()
        val id4 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val inAppMessageRealTimeLow = InAppMessageMocker.createInAppMessage(
            id = id4,
            priority = Priority.LOW,
            expireDate = expireDate,
            isRealTime = true
        )
        val inAppMessageNotRealTimeLow = InAppMessageMocker.createInAppMessage(
            id = id2,
            priority = Priority.LOW,
            expireDate = expireDate,
            isRealTime = false
        )
        val inAppMessageRealTimeHigh = InAppMessageMocker.createInAppMessage(
            id = id3,
            priority = Priority.HIGH,
            expireDate = expireDate,
            isRealTime = true
        )
        val inAppMessageNotRealTimeHigh = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            isRealTime = false
        )

        val inAppMessages = listOf(inAppMessageRealTimeLow, inAppMessageRealTimeHigh, inAppMessageNotRealTimeLow, inAppMessageNotRealTimeHigh)
        val sortedInAppMessages = inAppMessages.sortedWith(InAppMessageComparator())

        Assert.assertEquals(sortedInAppMessages[0].id, id1)
        Assert.assertEquals(sortedInAppMessages[1].id, id2)
        Assert.assertEquals(sortedInAppMessages[2].id, id3)
        Assert.assertEquals(sortedInAppMessages[3].id, id4)
    }

    @Test
    fun `operateScreenValues EQUALS test`() {
        val screenNameValue = "screenName"
        val screenName = "screenName"
        val operator = Operator.EQUALS.operator
        Assert.assertTrue(InAppMessageUtils.operateScreenValues(listOf(screenNameValue), screenName, operator))
    }

    @Test
    fun `operateScreenValues NOT_EQUALS test`() {
        val screenNameValue = "screenName"
        val screenName = "screenName"
        val operator = Operator.NOT_EQUALS.operator
        Assert.assertFalse(InAppMessageUtils.operateScreenValues(listOf(screenNameValue), screenName, operator))
    }

    @Test
    fun `operateScreenValues LIKE test`() {
        val screenNameValue = "screenName"
        val screenName = "screenNameLike"
        val operator = Operator.LIKE.operator
        Assert.assertTrue(InAppMessageUtils.operateScreenValues(listOf(screenNameValue), screenName, operator))
    }

    @Test
    fun `operateScreenValues NOT_LIKE test`() {
        val screenNameValue = "screenName"
        val screenName = "screenNameLike"
        val operator = Operator.NOT_LIKE.operator
        Assert.assertFalse(InAppMessageUtils.operateScreenValues(listOf(screenNameValue), screenName, operator))
    }

    @Test
    fun `operateScreenValues STARTS_WITH test`() {
        val screenNameValue = "screenName"
        val screenName = "screenNameStartsWith"
        val operator = Operator.STARTS_WITH.operator
        Assert.assertTrue(InAppMessageUtils.operateScreenValues(listOf(screenNameValue), screenName, operator))
    }

    @Test
    fun `operateScreenValues NOT_STARTS_WITH test`() {
        val screenNameValue = "screenName"
        val screenName = "screenNameStartsWith"
        val operator = Operator.NOT_STARTS_WITH.operator
        Assert.assertFalse(InAppMessageUtils.operateScreenValues(listOf(screenNameValue), screenName, operator))
    }

    @Test
    fun `operateScreenValues ENDS_WITH test`() {
        val screenNameValue = "EndsWith"
        val screenName = "screenNameEndsWith"
        val operator = Operator.ENDS_WITH.operator
        Assert.assertTrue(InAppMessageUtils.operateScreenValues(listOf(screenNameValue), screenName, operator))
    }

    @Test
    fun `operateScreenValues NOT_ENDS_WITH test`() {
        val screenNameValue = "EndsWith"
        val screenName = "screenNameEndsWith"
        val operator = Operator.NOT_ENDS_WITH.operator
        Assert.assertFalse(InAppMessageUtils.operateScreenValues(listOf(screenNameValue), screenName, operator))
    }

    @Test
    fun `operateScreenValues IN test`() {
        val screenName = "screenName1"
        val operator = Operator.IN.operator
        Assert.assertTrue(InAppMessageUtils.operateScreenValues(listOf("screenName1", "screenName2", "screenName3"),
            screenName, operator))
    }

    @Test
    fun `operateScreenValues NOT_IN test`() {
        val screenName = "screenName4"
        val operator = Operator.NOT_IN.operator
        Assert.assertTrue(InAppMessageUtils.operateScreenValues(listOf("screenName1", "screenName2", "screenName3"),
            screenName, operator))
    }

    @Test
    fun `findPriorRealTimeInApp category test`() {
        RealTimeInAppParamHolder.categoryPath = "Category"
        val id1 = Math.random().toString()
        val id2 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val criterionList = mutableListOf<Criterion>()
        criterionList.add(
            Criterion(
                id = 1,
                parameter = SpecialRuleParameter.CITY.key,
                dataType = DataType.TEXT.name,
                operator = Operator.EQUALS.operator,
                values = listOf("Ankara", "Istanbul")
            )
        )
        criterionList.add(
            Criterion(
                id = 2,
                parameter = SpecialRuleParameter.CATEGORY_PATH.key,
                dataType = DataType.TEXT.name,
                operator = Operator.LIKE.operator,
                values = listOf("Category")
            )
        )

        val realTimeInAppMessageMedium = InAppMessageMocker.createRealTimeInAppMessage(
            id = id1,
            priority = Priority.MEDIUM,
            expireDate = expireDate,
            criterionList = criterionList
        )
        val realTimeInAppMessageHigh = InAppMessageMocker.createRealTimeInAppMessage(
            id = id2,
            priority = Priority.HIGH,
            expireDate = expireDate,
            criterionList = criterionList
        )

        val inAppMessages = listOf(realTimeInAppMessageHigh, realTimeInAppMessageMedium)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages
        )
        Assert.assertEquals(priorInAppMessage?.id, id2)
    }
}