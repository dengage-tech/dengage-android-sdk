package com.dengage.sdk.push

import android.app.*
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.graphics.Bitmap
import android.media.AudioAttributes
import android.os.Build
import android.os.Bundle
import android.text.TextUtils
import android.util.Log
import android.widget.RemoteViews
import androidx.core.app.NotificationCompat
import androidx.core.content.ContextCompat
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.push.model.CarouselItem
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.domain.push.model.NotificationType
import com.dengage.sdk.util.*
import com.dengage.sdk.util.extension.toJson
import java.util.*

open class NotificationReceiver : BroadcastReceiver() {

    companion object {
        private const val TAG = "NotificationReceiver:"
    }

    override fun onReceive(context: Context, intent: Intent?) {
        DengageLogger.verbose("$TAG onReceive, intent action = ${intent?.action}")

        when (intent?.action) {
            Constants.PUSH_RECEIVE_EVENT -> onPushReceive(context, intent)
            Constants.PUSH_OPEN_EVENT -> onPushOpen(context, intent)
            Constants.PUSH_DELETE_EVENT -> onPushDismiss(context, intent)
            Constants.PUSH_ACTION_CLICK_EVENT -> onActionClick(context, intent)
            Constants.PUSH_ITEM_CLICK_EVENT -> onItemClick(context, intent)
        }
    }

    open fun onPushReceive(context: Context, intent: Intent) {
        ContextHolder.resetContext(context)
        DengageLogger.verbose("$TAG onPushReceive method is called")
        intent.extras ?: return
        prepareAndShowPush(context, intent)
    }

    open fun onPushOpen(context: Context, intent: Intent) {
        DengageLogger.verbose("$TAG onPushOpen method is called")

        var uri: String? = null
        if (intent.extras != null) {
            var message = Message.createFromIntent(intent.extras!!)
            val rawJson = intent.extras!!.getString("RAW_DATA")
            if (!TextUtils.isEmpty(rawJson)) {
                message = GsonHolder.gson.fromJson(rawJson, Message::class.java)
            }
            uri = intent.extras!!.getString("targetUrl")
            ContextHolder.context = context
            Dengage.sendOpenEvent("", "", message)

            clearNotification(context, message)
        } else {
            DengageLogger.error("$TAG No extra data for push open")
        }
        // context.launchActivity(intent, uri)
        // Log.d("oops","opened")
    }

    open fun onPushDismiss(context: Context, intent: Intent) {
        DengageLogger.verbose("$TAG onPushDismiss method is called")

        if (intent.extras != null) {
            var message = Message.createFromIntent(intent.extras!!)
            val rawJson = intent.extras!!.getString("RAW_DATA")
            if (!TextUtils.isEmpty(rawJson)) {
                message = GsonHolder.gson.fromJson(rawJson, Message::class.java)
            }

            clearNotification(context, message)
        }
    }

    open fun onActionClick(context: Context, intent: Intent) {
        DengageLogger.verbose("$TAG onActionClick method is called")

        var uri: String? = null
        if (intent.extras != null) {
            var message = Message.createFromIntent(intent.extras!!)
            val rawJson = intent.extras!!.getString("RAW_DATA")
            if (!TextUtils.isEmpty(rawJson)) {
                message = GsonHolder.gson.fromJson(rawJson, Message::class.java)
            }
            uri = intent.extras!!.getString("targetUrl")

            val id = intent.extras!!.getString("id", "")

            ContextHolder.context = context
            Dengage.sendOpenEvent(id, "", message)

            clearNotification(context, message)
        } else {

            DengageLogger.error("$TAG No extra data for push action")
        }

    }

    open fun onItemClick(context: Context, intent: Intent) {
        DengageLogger.verbose("$TAG onItemClick method is called")
        var navigation: String? = ""
        var uri: String? = null
        var id = ""
        var current: Int? = null
        var message: Message? = null
        val extras = intent.extras
        if (extras != null) {
            id = extras.getString("id", "")
            navigation = extras.getString("navigation")
            uri = extras.getString("targetUrl")
            current = extras.getInt("current")
            val rawJson = extras.getString("RAW_DATA")

            message = Message.createFromIntent(extras)
            if (!TextUtils.isEmpty(rawJson)) {
                message = GsonHolder.gson.fromJson(rawJson, Message::class.java)
            }
        } else {
            DengageLogger.error("$TAG No extra data for action")
        }

        DengageLogger.error("$TAG Current Item Index: $current")
        if (current != null && !message?.carouselContent.isNullOrEmpty()) {
            val item = message?.carouselContent?.get(current)
            uri = item?.targetUrl ?: ""
            id = item?.id ?: ""
            DengageLogger.debug("$TAG Current URI: ${item?.targetUrl}")
        }

        message?.let {
            ContextHolder.context = context
            when (navigation) {
                "" -> {
                    Dengage.sendOpenEvent("", id, message)
                    clearNotification(context, message)
                    // context.launchActivity(intent, uri)
                }
                "left", "right" -> {
                    if (message.carouselContent.isNullOrEmpty()) {
                        DengageLogger.error("$TAG carousel content is empty")
                    } else {
                        val size = message.carouselContent!!.size
                        val currentIndex = if (navigation == "right") {
                            ((current ?: 0) + 1) % size
                        } else {
                            ((current ?: 0) - 1 + size) % size
                        }
                        val right = (currentIndex + 1) % size
                        val left = (currentIndex - 1 + size) % size

                        intent.putExtra("current", currentIndex)
                        onCarouselRender(
                            context = context,
                            intent = intent,
                            message = message,
                            leftCarouselItem = message.carouselContent!![left],
                            currentCarouselItem = message.carouselContent!![currentIndex],
                            rightCarouselItem = message.carouselContent!![right],
                        )
                    }
                }
            }
        }
    }

    open fun onRichNotificationRender(
        context: Context,
        intent: Intent,
        message: Message,
        bitmap: Bitmap,
        notificationBuilder: NotificationCompat.Builder
    ) {
        val style = NotificationCompat.BigPictureStyle().bigPicture(bitmap)
        notificationBuilder.setLargeIcon(bitmap)
        notificationBuilder.setStyle(style)

        val manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager?
        val notification = notificationBuilder.build()
        manager?.notify(message.messageSource, message.messageId, notification)
    }

    open fun onTextNotificationRender(
        context: Context,
        intent: Intent,
        message: Message,
        notificationBuilder: NotificationCompat.Builder
    ) {
        val manager = context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager?
        val notification = notificationBuilder.build()
        manager?.notify(message.messageSource, message.messageId, notification)
    }

    protected open fun onCarouselRender(
        context: Context,
        intent: Intent,
        message: Message,
        leftCarouselItem: CarouselItem,
        currentCarouselItem: CarouselItem,
        rightCarouselItem: CarouselItem
    ) = Unit

    protected open fun getContentIntent(extras: Bundle?, packageName: String?): Intent {
        return Intent(Constants.PUSH_OPEN_EVENT).apply {
            if (extras != null) {
                putExtras(extras)
            }
            setPackage(packageName)
        }
    }

    protected open fun getDeleteIntent(extras: Bundle?, packageName: String?): Intent {
        return Intent(Constants.PUSH_DELETE_EVENT).apply {
            if (extras != null) {
                putExtras(extras)
            }
            setPackage(packageName)
        }
    }

    protected open fun getItemClickIntent(extras: Bundle?, packageName: String?): Intent {
        return Intent(Constants.PUSH_ITEM_CLICK_EVENT).apply {
            if (extras != null) {
                putExtras(extras)
            }
            putExtra("navigation", "")
            setPackage(packageName)
        }
    }

    protected open fun getLeftItemIntent(extras: Bundle?, packageName: String?): Intent {
        return Intent(Constants.PUSH_ITEM_CLICK_EVENT).apply {
            if (extras != null) {
                putExtras(extras)
            }
            putExtra("navigation", "left")
            setPackage(packageName)
        }
    }

    protected open fun getRightItemIntent(extras: Bundle?, packageName: String?): Intent {
        return Intent(Constants.PUSH_ITEM_CLICK_EVENT).apply {
            if (extras != null) {
                putExtras(extras)
            }
            putExtra("navigation", "right")
            setPackage(packageName)
        }
    }

    protected open fun loadCarouselImageToView(
        carouselView: RemoteViews,
        imageViewId: Int,
        carouselItem: CarouselItem,
        onComplete: (() -> Unit)? = null
    ) {
        val cachedFileBitmap = carouselItem.loadFileFromStorage()
        if (cachedFileBitmap == null) {
            ImageDownloadUtils.downloadImage(
                imageUrl = carouselItem.mediaUrl,
                onComplete = { bitmap ->
                    bitmap?.let {
                        carouselView.setImageViewBitmap(imageViewId, it)
                        onComplete?.invoke()
                    }
                }
            )
        } else {
            carouselView.setImageViewBitmap(imageViewId, cachedFileBitmap)
            onComplete?.invoke()
        }
    }

    open fun getPendingIntent(context: Context, requestCode: Int, intentP: Intent): PendingIntent? {
        var intent = intentP

        val extras = intentP.extras
        val packageName = context.packageName
        val action = intentP.action
        intent = Intent(context, NotificationNavigationDeciderActivity::class.java)
        intent.putExtras(extras!!)
        intent.setPackage(packageName)
        intent.action = action
        if (intent.extras != null) {
            intent.putExtras(intent.extras!!)
        }

        return PendingIntent.getActivity(
            context,
            requestCode,
            intent,
            PendingIntent.FLAG_IMMUTABLE or PendingIntent.FLAG_ONE_SHOT
        )

    }


    open fun getCarouselDirectionIntent(
        context: Context,
        requestCode: Int,
        intent: Intent
    ): PendingIntent? {

        return PendingIntent.getBroadcast(
            context,
            requestCode,
            intent,
            PendingIntent.FLAG_IMMUTABLE or PendingIntent.FLAG_UPDATE_CURRENT
        )
    }


    fun getDeletePendingIntent(
        context: Context,
        requestCode: Int,
        intentParam: Intent
    ): PendingIntent {

        return PendingIntent.getBroadcast(
            context,
            requestCode,
            intentParam,
            PendingIntent.FLAG_IMMUTABLE or PendingIntent.FLAG_UPDATE_CURRENT
        )

    }

    private fun prepareAndShowPush(context: Context?, intent: Intent?) {
        DengageLogger.verbose("$TAG prepareAndShowPush method is called")
        if (intent?.extras == null) {
            DengageLogger.verbose("$TAG prepareAndShowPush intent extras null")
            return
        }
        if (context == null) {
            DengageLogger.verbose("$TAG prepareAndShowPush context null")
            return
        }
        val message = Message.createFromIntent(intent.extras!!)
        when {
            message.notificationType === NotificationType.CAROUSEL -> {
                DengageLogger.verbose("$TAG this is a carousel notification")
                val imageUrls = message.carouselContent?.map {
                    it.mediaUrl
                }

                message.carouselContent?.let {
                    ImageDownloadUtils.downloadImages(
                        context = context,
                        imageUrls = imageUrls,
                        onComplete = { imageFileNames, imageFilePaths ->
                            message.carouselContent.forEachIndexed { index, carouselItem ->
                                carouselItem.mediaFileLocation = imageFilePaths[index]
                                carouselItem.mediaFileName = imageFileNames[index]
                            }
                            intent.putExtra("RAW_DATA", message.toJson())

                            if (message.carouselContent.isNullOrEmpty()) {
                                DengageLogger.error("$TAG carousel content is empty")
                            } else {
                                val size = message.carouselContent.size
                                val current = 0
                                val left = (current - 1 + size) % size
                                val right = (current + 1) % size

                                intent.putExtra("current", current)
                                onCarouselRender(
                                    context = context,
                                    intent = intent,
                                    message = message,
                                    leftCarouselItem = message.carouselContent[left],
                                    currentCarouselItem = message.carouselContent[current],
                                    rightCarouselItem = message.carouselContent[right],
                                )
                            }
                        }
                    )
                }
            }
            message.notificationType === NotificationType.RICH -> {
                DengageLogger.verbose("$TAG this is a rich notification")

                ImageDownloadUtils.downloadImage(
                    imageUrl = message.mediaUrl,
                    onComplete = { bitmap ->
                        bitmap?.let {
                            val notificationBuilder: NotificationCompat.Builder =
                                getNotificationBuilder(context, intent, message)
                            onRichNotificationRender(
                                context,
                                intent,
                                message,
                                bitmap,
                                notificationBuilder
                            )
                        }
                    }
                )
            }
            else -> {
                DengageLogger.verbose("$TAG this is a text notification")
                val notificationBuilder: NotificationCompat.Builder =
                    getNotificationBuilder(context, intent, message)
                onTextNotificationRender(context, intent, message, notificationBuilder)
            }
        }
    }

    private fun getNotificationBuilder(
        context: Context,
        intent: Intent,
        message: Message
    ): NotificationCompat.Builder {
        val extras = intent.extras
        val random = Random()

        val contentIntentRequestCode = random.nextInt()
        val deleteIntentRequestCode = random.nextInt()
        val packageName = context.packageName
        val contentIntent = getContentIntent(extras, packageName)
        val deleteIntent = getDeleteIntent(extras, packageName)
        val pContentIntent = getPendingIntent(context, contentIntentRequestCode, contentIntent)
        val pDeleteIntent = getDeletePendingIntent(context, deleteIntentRequestCode, deleteIntent)

        val channelId = createNotificationChannel(context, message)

        val notificationBuilder = NotificationCompat.Builder(context, channelId)
        notificationBuilder
            .setVibrate(longArrayOf(0, 100, 100, 100, 100, 100))
            .setContentIntent(pContentIntent)
            .setDeleteIntent(pDeleteIntent)
            .setAutoCancel(true)
            .setDefaults(Notification.DEFAULT_ALL)
            .setSmallIcon(context.getSmallIconId())
        val notificationSmallIconColorId: Int = context.getSmallIconColorId()
        if (notificationSmallIconColorId > 0) {
            notificationBuilder.color =
                ContextCompat.getColor(context, notificationSmallIconColorId)
        }
        if (!TextUtils.isEmpty(message.title)) {
            notificationBuilder.setContentTitle(message.title)
        }
        if (!TextUtils.isEmpty(message.subTitle)) {
            notificationBuilder.setSubText(message.subTitle)
        }
        if (!TextUtils.isEmpty(message.message)) {
            notificationBuilder.setStyle(NotificationCompat.BigTextStyle().bigText(message.message))
            notificationBuilder.setContentText(message.message)
        }
        notificationBuilder.setSound(message.sound.getSoundUri(context))
        if (message.badgeCount ?: 0 > 0) {
            notificationBuilder.setBadgeIconType(NotificationCompat.BADGE_ICON_SMALL)
            notificationBuilder.setNumber(message.badgeCount ?: 0)
        }
        if (!message.actionButtons.isNullOrEmpty()) {
            for (actionButton in message.actionButtons) {
                val requestCode = random.nextInt()
                val buttonIntent = Intent(Constants.PUSH_ACTION_CLICK_EVENT)
                buttonIntent.putExtra("id", actionButton.id)
                buttonIntent.putExtra("targetUrl", actionButton.targetUrl)
                buttonIntent.putExtra("RAW_DATA", message.toJson())
                buttonIntent.setPackage(packageName)
                val btnPendingIntent: PendingIntent? =
                    getPendingIntent(context, requestCode, buttonIntent)
                val icon: Int = context.getResourceId(actionButton.icon)
                notificationBuilder.addAction(icon, actionButton.text, btnPendingIntent)
            }
        }
        return notificationBuilder
    }

    open fun clearNotification(context: Context, message: Message) {
        context.clearNotification(message)
    }

    open fun createNotificationChannel(context: Context, message: Message): String {
        val soundUri = message.sound.getSoundUri(context)

        val channelId = Constants.NOTIFICATION_CHANNEL_ID

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            val notificationManager =
                context.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager

            val notificationChannel = NotificationChannel(
                channelId,
                Prefs.notificationChannelName,
                NotificationManager.IMPORTANCE_DEFAULT
            )
            val audioAttributes = AudioAttributes.Builder()
                .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
                .setUsage(AudioAttributes.USAGE_NOTIFICATION)
                .build()
            notificationChannel.setSound(soundUri, audioAttributes)
            notificationManager.createNotificationChannel(notificationChannel)
        }
        return channelId
    }
}