package com.dengage.sdk.push

import android.content.Context
import android.content.Intent
import android.graphics.Bitmap
import androidx.core.app.NotificationCompat
import androidx.core.app.Person
import androidx.core.content.pm.ShortcutInfoCompat
import androidx.core.content.pm.ShortcutManagerCompat
import androidx.core.graphics.drawable.IconCompat
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.extension.scale

/**
 * Shows a push as a conversation notification (the sender's name and image instead of the app's
 * title and icon) when the payload contains `senderName`. `senderImageUrl` is optional.
 *
 * On Android 11+ the notification is listed in the "Conversations" section because it uses
 * [NotificationCompat.MessagingStyle] and is linked to a long-lived shortcut for the sender.
 * Older versions show the MessagingStyle layout only.
 */
internal object CommunicationNotification {

    private const val TAG = "CommunicationNotification:"
    private const val SHORTCUT_ID_PREFIX = "dengage_conversation_"

    fun isCommunication(message: Message): Boolean = !message.senderName.isNullOrEmpty()

    fun apply(
        context: Context,
        notificationBuilder: NotificationCompat.Builder,
        message: Message,
        senderImage: Bitmap?
    ) {
        val senderName = message.senderName ?: return

        val senderIcon = senderImage?.let {
            try {
                IconCompat.createWithBitmap(it.scale())
            } catch (e: Exception) {
                DengageLogger.error("$TAG ${e.message}")
                null
            }
        }
        val sender = Person.Builder()
            .setName(senderName)
            .setKey(senderName)
            .setIcon(senderIcon)
            .build()
        // MessagingStyle requires a named user; it is not shown for incoming messages
        val user = Person.Builder()
            .setName(context.applicationInfo.loadLabel(context.packageManager))
            .build()

        val style = NotificationCompat.MessagingStyle(user)
            .addMessage(message.message.orEmpty(), System.currentTimeMillis(), sender)

        notificationBuilder
            .setStyle(style)
            .setCategory(NotificationCompat.CATEGORY_MESSAGE)

        publishShortcut(context, senderName, sender, senderIcon)?.let {
            notificationBuilder.setShortcutId(it)
        }
    }

    private fun publishShortcut(
        context: Context,
        senderName: String,
        sender: Person,
        senderIcon: IconCompat?
    ): String? {
        return try {
            val launchIntent = context.packageManager.getLaunchIntentForPackage(context.packageName)
                ?: Intent(Intent.ACTION_MAIN).setPackage(context.packageName)
            val shortcutId = SHORTCUT_ID_PREFIX + senderName
            val shortcut = ShortcutInfoCompat.Builder(context, shortcutId)
                .setShortLabel(senderName)
                .setLongLived(true)
                .setPerson(sender)
                .setIcon(senderIcon ?: IconCompat.createWithResource(context, context.applicationInfo.icon))
                .setIntent(launchIntent)
                .build()
            ShortcutManagerCompat.pushDynamicShortcut(context, shortcut)
            shortcutId
        } catch (e: Exception) {
            DengageLogger.error("$TAG could not publish conversation shortcut: ${e.message}")
            null
        }
    }
}
