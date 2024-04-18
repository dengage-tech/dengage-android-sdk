package com.dengage.android.kotlin.sample.push

import android.app.Notification
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.os.Build
import android.util.Log
import android.widget.RemoteViews
import androidx.core.app.NotificationCompat
import androidx.core.app.NotificationManagerCompat
import com.dengage.android.kotlin.sample.R
import com.dengage.sdk.domain.push.model.CarouselItem
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.push.NotificationReceiver
import com.dengage.sdk.push.getSoundUri

class PushNotificationReceiver : NotificationReceiver() {

    override fun onCarouselRender(
        context: Context,
        intent: Intent,
        message: Message,
        leftCarouselItem: CarouselItem,
        currentCarouselItem: CarouselItem,
        rightCarouselItem: CarouselItem
    ) {
        super.onCarouselRender(
            context,
            intent,
            message,
            leftCarouselItem,
            currentCarouselItem,
            rightCarouselItem
        )

        val itemTitle = currentCarouselItem.title
        val itemDesc = currentCarouselItem.description

        // set intents (right button, left button, item click)
        val itemIntent = getItemClickIntent(intent.extras, context.packageName)
        val leftIntent = getLeftItemIntent(intent.extras, context.packageName)
        val rightIntent = getRightItemIntent(intent.extras, context.packageName)
        val deleteIntent = getDeleteIntent(intent.extras, context.packageName)
        val contentIntent = getContentIntent(intent.extras, context.packageName)

        val carouseItemIntent = getPendingIntent(context, 0, itemIntent)
        val carouselLeftIntent = getCarouselDirectionIntent(context, 1, leftIntent)
        val carouselRightIntent = getCarouselDirectionIntent(context, 2, rightIntent)
        val deletePendingIntent = getDeletePendingIntent(context, 4, deleteIntent)
        val contentPendingIntent = getPendingIntent(context, 5, contentIntent)

        // set views for the layout
        val collapsedView = RemoteViews(
            context.packageName,
            R.layout.den_carousel_collapsed
        )
        collapsedView.setTextViewText(R.id.den_carousel_title, message.title)
        collapsedView.setTextViewText(R.id.den_carousel_message, message.message)

        val carouselView = RemoteViews(
            context.packageName,
            R.layout.den_carousel_portrait
        )
        carouselView.setTextViewText(R.id.den_carousel_title, message.title)
        carouselView.setTextViewText(R.id.den_carousel_message, message.message)
        carouselView.setTextViewText(R.id.den_carousel_item_title, itemTitle)
        carouselView.setTextViewText(R.id.den_carousel_item_description, itemDesc)

        carouselView.setOnClickPendingIntent(R.id.den_carousel_left_arrow, carouselLeftIntent)
        carouselView.setOnClickPendingIntent(
            R.id.den_carousel_portrait_current_image,
            carouseItemIntent
        )
        carouselView.setOnClickPendingIntent(R.id.den_carousel_item_title, carouseItemIntent)
        carouselView.setOnClickPendingIntent(R.id.den_carousel_item_description, carouseItemIntent)
        carouselView.setOnClickPendingIntent(R.id.den_carousel_right_arrow, carouselRightIntent)

        val channelId = createNotificationChannel(context, message)

        loadCarouselImageToView(
            carouselView = carouselView,
            imageViewId = R.id.den_carousel_portrait_left_image,
            carouselItem = leftCarouselItem,
            onComplete = {
                // you can call notificationManager.notify for devices that could not show carousel image contents
            }
        )

        loadCarouselImageToView(
            carouselView = carouselView,
            imageViewId = R.id.den_carousel_portrait_current_image,
            carouselItem = currentCarouselItem,
            onComplete = {
                // you can call notificationManager.notify for devices that could not show carousel image contents
            }
        )

        loadCarouselImageToView(
            carouselView = carouselView,
            imageViewId = R.id.den_carousel_portrait_right_image,
            carouselItem = rightCarouselItem,
            onComplete = {
                // you can call notificationManager.notify for devices that could not show carousel image contents
            }
        )

        val notification = NotificationCompat.Builder(context, channelId)
            .setSmallIcon(R.mipmap.ic_launcher)
            .setCustomContentView(collapsedView)
            .setCustomBigContentView(carouselView)
            .setContentIntent(contentPendingIntent)
            .setDeleteIntent(deletePendingIntent)
            .setSound(message.sound.getSoundUri(context))
            .build()

        // show message again silently with next, previous and current item.
        notification.flags = Notification.FLAG_AUTO_CANCEL or Notification.FLAG_ONLY_ALERT_ONCE

        // show message
        val notificationManager = NotificationManagerCompat.from(context)
        intent.extras?.getInt("requestCode")?.let { notificationManager.notify(it, notification) }
    }
}