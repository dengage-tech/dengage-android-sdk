package com.dengage.sdk.push

import android.content.Context
import android.content.Intent
import android.graphics.Bitmap
import android.net.Uri
import android.os.Build
import android.graphics.Typeface
import android.text.SpannableStringBuilder
import android.text.Spanned
import android.text.style.StyleSpan
import androidx.core.app.NotificationCompat
import androidx.core.app.Person
import androidx.core.content.pm.ShortcutInfoCompat
import androidx.core.content.FileProvider
import androidx.core.content.pm.ShortcutManagerCompat
import androidx.core.graphics.drawable.IconCompat
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.ImageDownloadUtils
import com.dengage.sdk.util.extension.scale
import java.io.File
import java.io.FileOutputStream
import java.util.concurrent.TimeUnit

/**
 * Shows a push as a conversation notification when the payload contains `notificationIcon`: the
 * image at `notificationIcon` as the sender's avatar and the title (or the app name, when the title
 * is empty) as the sender's name. Pushes with the same `notificationIcon` share one conversation.
 * The image at `mediaUrl`, if any, is shown above the message (Android 9+).
 *
 * On Android 11+ the notification is listed in the "Conversations" section because it uses
 * [NotificationCompat.MessagingStyle] and is linked to a long-lived shortcut for the sender.
 * Older versions show the MessagingStyle layout only.
 */
internal object CommunicationNotification {

    private const val TAG = "CommunicationNotification:"
    private const val SHORTCUT_ID_PREFIX = "dengage_conversation_"
    private const val IMAGE_DIR = "dengage_push_images"
    private const val IMAGE_MIME_TYPE = "image/png"
    private val IMAGE_MAX_AGE_MS = TimeUnit.DAYS.toMillis(7)

    fun isCommunication(message: Message): Boolean = !message.notificationIcon.isNullOrEmpty()

    /**
     * Downloads `notificationIcon` and `mediaUrl`, and passes the icon and a content uri of the
     * image (readable by the system UI) to [onComplete] on a background thread.
     */
    fun loadImages(context: Context, message: Message, onComplete: (icon: Bitmap?, image: Uri?) -> Unit) {
        ImageDownloadUtils.downloadImage(message.notificationIcon) { icon ->
            if (message.mediaUrl.isNullOrEmpty()) {
                onComplete(icon, null)
            } else {
                ImageDownloadUtils.downloadImage(message.mediaUrl) { image ->
                    onComplete(icon, image?.let { shareImage(context, it) })
                }
            }
        }
    }

    fun apply(
        context: Context,
        notificationBuilder: NotificationCompat.Builder,
        message: Message,
        icon: Bitmap?,
        image: Uri? = null
    ) {
        val notificationIcon = message.notificationIcon ?: return
        val appName = context.applicationInfo.loadLabel(context.packageManager)
        val senderName = message.title.takeUnless { it.isNullOrEmpty() } ?: appName

        val senderIcon = icon?.let {
            try {
                IconCompat.createWithBitmap(it.scale())
            } catch (e: Exception) {
                DengageLogger.error("$TAG ${e.message}")
                null
            }
        }
        val sender = Person.Builder()
            .setName(senderName)
            .setKey(notificationIcon)
            .setIcon(senderIcon)
            .build()
        // MessagingStyle requires a named user; it is not shown for incoming messages
        val user = Person.Builder()
            .setName(appName)
            .build()

        val text = messageText(message)
        val time = System.currentTimeMillis()
        val style = NotificationCompat.MessagingStyle(user)
        // An image message shows only the image, so the text follows it as the latest message,
        // which is the one shown when collapsed; images need Android 9+
        if (image != null && Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
            style.addMessage(
                NotificationCompat.MessagingStyle.Message(text, time, sender).setData(IMAGE_MIME_TYPE, image)
            )
        }
        style.addMessage(text, time, sender)

        notificationBuilder
            .setStyle(style)
            .setCategory(NotificationCompat.CATEGORY_MESSAGE)

        publishShortcut(context, notificationIcon, senderName, sender, senderIcon)?.let {
            notificationBuilder.setShortcutId(it)
        }
    }

    /**
     * The conversation template does not show the sub text or the conversation title of a
     * one-to-one conversation, so `subTitle` is shown in bold before the message.
     */
    internal fun messageText(message: Message): CharSequence {
        val text = message.message.orEmpty()
        val subTitle = message.subTitle
        if (subTitle.isNullOrEmpty()) return text
        return SpannableStringBuilder()
            .append(subTitle, StyleSpan(Typeface.BOLD), Spanned.SPAN_EXCLUSIVE_EXCLUSIVE)
            .apply { if (text.isNotEmpty()) append(" · ").append(text) }
    }

    private fun shareImage(context: Context, image: Bitmap): Uri? {
        return try {
            val dir = File(context.cacheDir, IMAGE_DIR).apply { mkdirs() }
            val now = System.currentTimeMillis()
            dir.listFiles()?.filter { now - it.lastModified() > IMAGE_MAX_AGE_MS }?.forEach { it.delete() }

            val file = File(dir, "$now.png")
            FileOutputStream(file).use { image.compress(Bitmap.CompressFormat.PNG, 100, it) }
            FileProvider.getUriForFile(context, "${context.packageName}.dengage.fileprovider", file)
        } catch (e: Exception) {
            DengageLogger.error("$TAG could not share image: ${e.message}")
            null
        }
    }

    private fun publishShortcut(
        context: Context,
        notificationIcon: String,
        senderName: CharSequence,
        sender: Person,
        senderIcon: IconCompat?
    ): String? {
        return try {
            val launchIntent = context.packageManager.getLaunchIntentForPackage(context.packageName)
                ?: Intent(Intent.ACTION_MAIN).setPackage(context.packageName)
            val shortcutId = SHORTCUT_ID_PREFIX + notificationIcon
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
