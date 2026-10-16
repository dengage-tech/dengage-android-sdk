package com.dengage.sdk.push

import android.graphics.Bitmap
import android.net.Uri
import androidx.core.app.NotificationCompat
import androidx.core.content.pm.ShortcutManagerCompat
import androidx.test.platform.app.InstrumentationRegistry
import com.dengage.sdk.domain.push.model.Message
import org.junit.Assert
import org.junit.Test
import org.junit.runner.RunWith
import org.robolectric.RobolectricTestRunner
import org.robolectric.annotation.Config

@RunWith(RobolectricTestRunner::class)
@Config(sdk = [33])
class CommunicationNotificationTest {

    private val context = InstrumentationRegistry.getInstrumentation().targetContext

    private fun message(notificationIcon: String?, title: String = "Dengage") = Message.createFromMap(
        mapOf(
            "messageSource" to "DENGAGE",
            "title" to title,
            "message" to "Favorilediğin üründe geçerli 100 TL kuponun bugün son günü!",
            "notificationIcon" to notificationIcon
        )
    )

    private fun applyAndBuild(message: Message) = NotificationCompat.Builder(context, "channel")
        .setSmallIcon(android.R.drawable.ic_dialog_info)
        .also {
            CommunicationNotification.apply(
                context, it, message, Bitmap.createBitmap(64, 64, Bitmap.Config.ARGB_8888)
            )
        }
        .build()

    @Test
    fun `createFromMap parses notificationIcon`() {
        val message = message(ICON_URL)

        Assert.assertEquals(ICON_URL, message.notificationIcon)
        Assert.assertTrue(CommunicationNotification.isCommunication(message))
    }

    @Test
    fun `push without notificationIcon is not a communication notification`() {
        Assert.assertFalse(CommunicationNotification.isCommunication(message(null)))
        Assert.assertFalse(CommunicationNotification.isCommunication(message("")))
    }

    @Test
    fun `apply builds messaging style linked to a shortcut for the icon`() {
        val message = message(ICON_URL)
        val notification = applyAndBuild(message)

        val style = NotificationCompat.MessagingStyle
            .extractMessagingStyleFromNotification(notification)
        Assert.assertNotNull(style)
        val styleMessage = style!!.messages.single()
        Assert.assertEquals(message.message, styleMessage.text.toString())
        Assert.assertEquals("Dengage", styleMessage.person?.name.toString())
        Assert.assertEquals(ICON_URL, styleMessage.person?.key)
        Assert.assertNotNull(styleMessage.person?.icon)
        Assert.assertEquals(NotificationCompat.CATEGORY_MESSAGE, notification.category)

        val shortcutId = "dengage_conversation_$ICON_URL"
        Assert.assertEquals(shortcutId, notification.shortcutId)
        val shortcut = ShortcutManagerCompat.getDynamicShortcuts(context)
            .single { it.id == shortcutId }
        Assert.assertEquals("Dengage", shortcut.shortLabel.toString())
    }

    @Test
    fun `sender name falls back to the app name when the title is empty`() {
        val notification = applyAndBuild(message(ICON_URL, title = ""))

        val styleMessage = NotificationCompat.MessagingStyle
            .extractMessagingStyleFromNotification(notification)!!.messages.single()
        Assert.assertEquals(
            context.applicationInfo.loadLabel(context.packageManager).toString(),
            styleMessage.person?.name.toString()
        )
    }

    @Test
    fun `image is added as a message before the text`() {
        val message = message(ICON_URL)
        val image = Uri.parse("content://com.example.dengage.fileprovider/dengage_push_images/1.png")
        val builder = NotificationCompat.Builder(context, "channel")
            .setSmallIcon(android.R.drawable.ic_dialog_info)
        CommunicationNotification.apply(context, builder, message, null, image)

        val messages = NotificationCompat.MessagingStyle
            .extractMessagingStyleFromNotification(builder.build())!!.messages
        Assert.assertEquals(2, messages.size)
        Assert.assertEquals(image, messages[0].dataUri)
        Assert.assertEquals("image/png", messages[0].dataMimeType)
        Assert.assertNull(messages[1].dataUri)
        Assert.assertEquals(message.message, messages[1].text.toString())
    }

    @Test
    fun `subTitle is shown before the message`() {
        val message = Message.createFromMap(
            mapOf("message" to "egemen-test mesaj", "subTitle" to "trendyol", "notificationIcon" to ICON_URL)
        )

        Assert.assertEquals("trendyol · egemen-test mesaj", CommunicationNotification.messageText(message).toString())
        val withoutSubTitle = message(ICON_URL)
        Assert.assertEquals(withoutSubTitle.message, CommunicationNotification.messageText(withoutSubTitle).toString())
    }

    private companion object {
        const val ICON_URL = "https://example.com/icon.jpg"
    }
}
