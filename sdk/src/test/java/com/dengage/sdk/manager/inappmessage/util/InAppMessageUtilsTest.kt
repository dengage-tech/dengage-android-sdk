package com.dengage.sdk.manager.inappmessage.util

import org.junit.Assert
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import java.text.SimpleDateFormat
import java.util.*
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config
import androidx.test.platform.app.InstrumentationRegistry
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.Criterion
import com.dengage.sdk.domain.inappmessage.model.DataType
import com.dengage.sdk.domain.inappmessage.model.Operator
import com.dengage.sdk.domain.inappmessage.model.Priority
import com.dengage.sdk.domain.inappmessage.model.SpecialRuleParameter
import com.dengage.sdk.domain.subscription.model.Subscription
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.ContextHolder


@Config(manifest=Config.NONE)
@RunWith(RobolectricTestRunner::class)
class InAppMessageUtilsTest {

    @Before
    fun setup() {
        ContextHolder.context = InstrumentationRegistry.getInstrumentation().context
    }

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
    fun `findPriorInAppMessage display timing max show count test`() {
        val id1 = Math.random().toString()
        val id2 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val inAppMessageIsAvailableTiming = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.MEDIUM,
            expireDate = expireDate,
            maxShowCount = 2
        )
        val inAppMessageIsNotAvailableTiming = InAppMessageMocker.createInAppMessage(
            id = id2,
            priority = Priority.HIGH,
            expireDate = expireDate,
            maxShowCount = 1
        )
        inAppMessageIsAvailableTiming.data.showCount = 1
        inAppMessageIsNotAvailableTiming.data.showCount = 1

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
        val id5 = Math.random().toString()

        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val inAppMessageRealTimeLowHasNoRules = InAppMessageMocker.createInAppMessage(
            id = id5,
            priority = Priority.LOW,
            expireDate = expireDate,
            isRealTime = true,
            hasRules = false
        )
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

        val inAppMessages = listOf(inAppMessageRealTimeLowHasNoRules, inAppMessageRealTimeLow, inAppMessageRealTimeHigh, inAppMessageNotRealTimeLow, inAppMessageNotRealTimeHigh)
        val sortedInAppMessages = inAppMessages.sortedWith(InAppMessageComparator())

        Assert.assertEquals(sortedInAppMessages[0].id, id1)
        Assert.assertEquals(sortedInAppMessages[1].id, id2)
        Assert.assertEquals(sortedInAppMessages[2].id, id3)
        Assert.assertEquals(sortedInAppMessages[3].id, id4)
        Assert.assertEquals(sortedInAppMessages[4].id, id5)
    }

    @Test
    fun `operateScreenValues EQUALS test through findPriorInAppMessage`() {
        val id1 = Math.random().toString()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val inAppMessage = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "testScreen",
            operator = Operator.EQUALS
        )

        val inAppMessages = listOf(inAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "testScreen"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `operateScreenValues NOT_EQUALS test through findPriorInAppMessage`() {
        val id1 = Math.random().toString()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val inAppMessage = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "testScreen",
            operator = Operator.NOT_EQUALS
        )

        val inAppMessages = listOf(inAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "differentScreen"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `operateScreenValues LIKE test through findPriorInAppMessage`() {
        val id1 = Math.random().toString()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val inAppMessage = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "test",
            operator = Operator.LIKE
        )

        val inAppMessages = listOf(inAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "testScreenLike"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `operateScreenValues NOT_LIKE test through findPriorInAppMessage`() {
        val id1 = Math.random().toString()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val inAppMessage = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "test",
            operator = Operator.NOT_LIKE
        )

        val inAppMessages = listOf(inAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "differentScreen"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `operateScreenValues STARTS_WITH test through findPriorInAppMessage`() {
        val id1 = Math.random().toString()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val inAppMessage = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "test",
            operator = Operator.STARTS_WITH
        )

        val inAppMessages = listOf(inAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "testScreenStartsWith"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `operateScreenValues NOT_STARTS_WITH test through findPriorInAppMessage`() {
        val id1 = Math.random().toString()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val inAppMessage = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "test",
            operator = Operator.NOT_STARTS_WITH
        )

        val inAppMessages = listOf(inAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "differentScreen"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `operateScreenValues ENDS_WITH test through findPriorInAppMessage`() {
        val id1 = Math.random().toString()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val inAppMessage = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "EndsWith",
            operator = Operator.ENDS_WITH
        )

        val inAppMessages = listOf(inAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "testScreenEndsWith"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `operateScreenValues NOT_ENDS_WITH test through findPriorInAppMessage`() {
        val id1 = Math.random().toString()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val inAppMessage = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "EndsWith",
            operator = Operator.NOT_ENDS_WITH
        )

        val inAppMessages = listOf(inAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "differentScreen"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `operateScreenValues IN test through findPriorInAppMessage`() {
        // Note: The current InAppMessageMocker doesn't support multiple screen names for IN operator
        // This test assumes the screen name filter value is set to the screenName parameter
        val id1 = Math.random().toString()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val inAppMessage = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "screenName1",
            operator = Operator.IN
        )

        val inAppMessages = listOf(inAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "screenName1"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `operateScreenValues NOT_IN test through findPriorInAppMessage`() {
        val id1 = Math.random().toString()
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())

        val inAppMessage = InAppMessageMocker.createInAppMessage(
            id = id1,
            priority = Priority.HIGH,
            expireDate = expireDate,
            screenName = "screenName1",
            operator = Operator.NOT_IN
        )

        val inAppMessages = listOf(inAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages,
            screenName = "screenName4"
        )
        Assert.assertEquals(priorInAppMessage?.id, id1)
    }

    @Test
    fun `findPriorRealTimeInApp category test`() {
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
                values = listOf("Ankara", "Istanbul"),
                valueSource = "SERVER_SIDE",
                //type = null,
                aggregateType = null,
                aggregateField = null,
                eventType = null,
                timeWindow = null,
                filtersLogicalOp = null,
                filters = null
            )
        )
        criterionList.add(
            Criterion(
                id = 2,
                parameter = SpecialRuleParameter.CATEGORY_PATH.key,
                dataType = DataType.TEXT.name,
                operator = Operator.LIKE.operator,
                values = listOf("Category"),
                valueSource = "SERVER_SIDE",
                //type = null,
                aggregateType = null,
                aggregateField = null,
                eventType = null,
                timeWindow = null,
                filtersLogicalOp = null,
                filters = null
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

        Dengage.setCity("Ankara")
        Dengage.setCategoryPath("Category")

        val inAppMessages = listOf(realTimeInAppMessageHigh, realTimeInAppMessageMedium)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages
        )



        Assert.assertEquals(priorInAppMessage?.id, id2)
    }

    @Test
    fun `findPriorRealTimeInApp anonymous true when contact key is empty set test`() {
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val criterionList = mutableListOf<Criterion>()
        criterionList.add(
            Criterion(
                id = 1,
                parameter = SpecialRuleParameter.ANONYMOUS.key,
                dataType = DataType.BOOL.name,
                operator = Operator.EQUALS.operator,
                values = listOf("true"),
                valueSource = "SERVER_SIDE"
            )
        )
        val realTimeInAppMessage = InAppMessageMocker.createRealTimeInAppMessage(
            id = Math.random().toString(),
            priority = Priority.MEDIUM,
            expireDate = expireDate,
            criterionList = criterionList
        )
        val inAppMessages = listOf(realTimeInAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages
        )
        Assert.assertNotNull(priorInAppMessage)
    }

    @Test
    fun `findPriorRealTimeInApp anonymous true when contact key is not empty test`() {
        Prefs.subscription = Subscription(
            contactKey = "testContactKey"
        )
        val expireDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
        val expireDate = expireDateFormat.format(Date())
        val criterionList = mutableListOf<Criterion>()
        criterionList.add(
            Criterion(
                id = 1,
                parameter = SpecialRuleParameter.ANONYMOUS.key,
                dataType = DataType.BOOL.name,
                operator = Operator.EQUALS.operator,
                values = listOf("false"),
                valueSource = "SERVER_SIDE"
            )
        )
        val realTimeInAppMessage = InAppMessageMocker.createRealTimeInAppMessage(
            id = Math.random().toString(),
            priority = Priority.MEDIUM,
            expireDate = expireDate,
            criterionList = criterionList
        )
        val inAppMessages = listOf(realTimeInAppMessage)
        val priorInAppMessage = InAppMessageUtils.findPriorInAppMessage(
            inAppMessages = inAppMessages
        )
        Assert.assertNotNull(priorInAppMessage)
    }

}
