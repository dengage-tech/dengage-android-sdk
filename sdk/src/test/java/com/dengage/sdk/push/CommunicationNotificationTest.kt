package com.dengage.sdk.push

import android.graphics.Bitmap
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

    private fun message(senderName: String?) = Message.createFromMap(
        mapOf(
            "messageSource" to "DENGAGE",
            "title" to "[TEST] communication notification",
            "message" to "Favorilediğin üründe geçerli 100 TL kuponun bugün son günü!",
            "senderName" to senderName,
            "senderImageUrl" to "https://example.com/avatar.jpg"
        )
    )

    @Test
    fun `createFromMap parses sender fields`() {
        val message = message("Dengage")

        Assert.assertEquals("Dengage", message.senderName)
        Assert.assertEquals("https://example.com/avatar.jpg", message.senderImageUrl)
        Assert.assertTrue(CommunicationNotification.isCommunication(message))
    }

    @Test
    fun `push without senderName is not a communication notification`() {
        Assert.assertFalse(CommunicationNotification.isCommunication(message(null)))
        Assert.assertFalse(CommunicationNotification.isCommunication(message("")))
    }

    @Test
    fun `apply builds messaging style linked to a sender shortcut`() {
        val message = message("Dengage")
        val builder = NotificationCompat.Builder(context, "channel")
            .setSmallIcon(android.R.drawable.ic_dialog_info)
        val senderImage = Bitmap.createBitmap(64, 64, Bitmap.Config.ARGB_8888)

        CommunicationNotification.apply(context, builder, message, senderImage)
        val notification = builder.build()

        val style = NotificationCompat.MessagingStyle
            .extractMessagingStyleFromNotification(notification)
        Assert.assertNotNull(style)
        val styleMessage = style!!.messages.single()
        Assert.assertEquals(message.message, styleMessage.text.toString())
        Assert.assertEquals("Dengage", styleMessage.person?.name.toString())
        Assert.assertNotNull(styleMessage.person?.icon)
        Assert.assertEquals(NotificationCompat.CATEGORY_MESSAGE, notification.category)

        Assert.assertEquals("dengage_conversation_Dengage", notification.shortcutId)
        val shortcut = ShortcutManagerCompat.getDynamicShortcuts(context)
            .single { it.id == "dengage_conversation_Dengage" }
        Assert.assertEquals("Dengage", shortcut.shortLabel.toString())
    }
}
