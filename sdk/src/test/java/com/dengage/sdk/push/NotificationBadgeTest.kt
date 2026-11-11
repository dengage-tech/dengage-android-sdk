package com.dengage.sdk.push

import android.app.NotificationChannel
import android.app.NotificationManager
import android.content.Context
import androidx.core.app.NotificationCompat
import androidx.test.core.app.ApplicationProvider
import com.dengage.sdk.domain.push.model.Message
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

@Config(manifest = Config.NONE, sdk = [33])
@RunWith(RobolectricTestRunner::class)
class NotificationBadgeTest {

    private lateinit var context: Context
    private lateinit var manager: NotificationManager

    @Before
    fun setup() {
        context = ApplicationProvider.getApplicationContext()
        manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        manager.createNotificationChannel(NotificationChannel(CHANNEL, CHANNEL, NotificationManager.IMPORTANCE_DEFAULT))
        manager.cancelAll()
    }

    @Test
    fun `isIncrementalBadge is parsed from 1, true and True`() {
        listOf("1", "true", "True", " TRUE ").forEach {
            Assert.assertTrue(it, message(isIncrementalBadge = it).isIncrementalBadge == true)
        }
        listOf("0", "false", "False", "", null, "abc").forEach {
            Assert.assertTrue("$it", message(isIncrementalBadge = it).isIncrementalBadge == false)
        }
    }

    @Test
    fun `incremental push adds one to the current badge`() {
        postActive(id = 10, number = 3)

        Assert.assertEquals(1, numberFor(message(isIncrementalBadge = "1", badgeCount = "3")))
    }

    @Test
    fun `badgeCount sets the badge to that value`() {
        Assert.assertEquals(3, numberFor(message(badgeCount = "3")))

        postActive(id = 10, number = 2)
        postActive(id = 11, number = 0) // counts as 1
        Assert.assertEquals(2, numberFor(message(badgeCount = "5")))
    }

    @Test
    fun `badgeCount lower than the current badge adds the minimum of one`() {
        postActive(id = 10, number = 4)

        Assert.assertEquals(1, numberFor(message(badgeCount = "2")))
    }

    @Test
    fun `notification replaced with the same id is not counted`() {
        postActive(id = 7, number = 3)

        Assert.assertEquals(3, numberFor(message(badgeCount = "3"), notificationId = 7))
    }

    @Test
    fun `no badgeCount keeps the default number`() {
        Assert.assertEquals(0, numberFor(message(badgeCount = "0")))
        Assert.assertEquals(0, numberFor(message()))
    }

    private fun numberFor(message: Message, notificationId: Int = 99): Int {
        val builder = NotificationCompat.Builder(context, CHANNEL)
        NotificationBadge.apply(context, builder, message, notificationId)
        return builder.build().number
    }

    private fun postActive(id: Int, number: Int) {
        val notification = NotificationCompat.Builder(context, CHANNEL)
            .setSmallIcon(android.R.drawable.ic_dialog_info)
            .setNumber(number)
            .build()
        manager.notify(id, notification)
    }

    private fun message(isIncrementalBadge: String? = null, badgeCount: String? = null): Message {
        val map = mutableMapOf<String, String?>("messageSource" to "DENGAGE", "notificationType" to "TEXT")
        isIncrementalBadge?.let { map["isIncrementalBadge"] = it }
        badgeCount?.let { map["badgeCount"] = it }
        return Message.createFromMap(map)
    }

    private companion object {
        const val CHANNEL = "test"
    }
}
