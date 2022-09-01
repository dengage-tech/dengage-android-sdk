package com.dengage.sdk.push

import android.R.drawable
import android.app.Activity
import android.app.NotificationManager
import android.app.TaskStackBuilder
import android.content.ContentResolver
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import android.media.RingtoneManager
import android.net.Uri
import android.os.Build
import android.text.TextUtils
import androidx.core.app.NotificationManagerCompat
import androidx.core.text.isDigitsOnly
import com.dengage.sdk.domain.push.model.CarouselItem
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import java.io.File
import java.io.FileInputStream

fun String?.getSoundUri(context: Context): Uri {
    val id = if (TextUtils.isEmpty(this)) {
        0
    } else {
        context.resources.getIdentifier(this, "raw", context.packageName)
    }
    return if (id != 0) {
        Uri.parse(ContentResolver.SCHEME_ANDROID_RESOURCE + "://" + context.packageName + "/" + id)
    } else {
        RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION)
    }
}

fun Context.getSmallIconId(): Int {
    return try {
        val packageManager = this.packageManager
        val smallIcon = DengageUtils.getMetaData(
            context = this,
            name = "den_push_small_icon"
        )
        if (!TextUtils.isEmpty(smallIcon)) {
            val appIconId: Int = this.getResourceId(smallIcon)
            DengageLogger.verbose("Application icon: $smallIcon")
            appIconId
        } else {
            val applicationInfo =
                packageManager.getApplicationInfo(this.packageName, PackageManager.GET_META_DATA)
            DengageLogger.verbose("Application icon: " + applicationInfo.icon)
            applicationInfo.icon
        }
    } catch (e: PackageManager.NameNotFoundException) {
        DengageLogger.verbose("Application Icon Not Found")
        -1
    }
}

fun Context.getSmallIconColorId(): Int {
    val smallIcon = DengageUtils.getMetaData(
        context = this,
        name = "den_push_small_icon_color"
    )
    return if (!TextUtils.isEmpty(smallIcon)) {
        val appIconColorId = this.getColorResourceId(smallIcon)
        DengageLogger.verbose("Application icon: $smallIcon")
        appIconColorId
    } else {
        -1 // in case metadata not provided in AndroidManifest
    }
}

fun Context.getColorResourceId(resourceName: String?): Int {
    if (TextUtils.isEmpty(resourceName)) return 0
    return if (resourceName!!.isDigitsOnly()) 0 else try {
        this.resources.getIdentifier(resourceName, "color", this.packageName)
    } catch (e: Exception) {
        try {
            return drawable::class.java.getField(resourceName).getInt(null)
        } catch (ignored: Throwable) {
            DengageLogger.verbose("Color resource id not found $resourceName")
        }
        e.printStackTrace()
        0
    }
}

fun Context.getResourceId(resourceName: String?): Int {
    if (TextUtils.isEmpty(resourceName)) return 0
    return if (resourceName!!.isDigitsOnly()) 0 else try {
        this.resources.getIdentifier(resourceName, "drawable", this.packageName)
    } catch (e: Exception) {
        try {
            return drawable::class.java.getField(resourceName).getInt(null)
        } catch (ignored: Throwable) {
            DengageLogger.verbose("Resource id not found $resourceName")
        }
        e.printStackTrace()
        0
    }
}

fun CarouselItem.removeFileFromStorage(): Boolean {
    val file = File(mediaFileLocation, "$mediaFileName.png")
    return if (file.exists()) {
        file.delete()
    } else {
        false
    }
}

fun CarouselItem.loadFileFromStorage(): Bitmap? {
    val file = File(mediaFileLocation, "$mediaFileName.png")
    return if (file.exists()) {
        BitmapFactory.decodeStream(FileInputStream(file))
    } else {
        null
    }
}

fun Context.launchActivity(intent: Intent?, uri: String?) {
    val clazz: Class<out Activity?> = getActivity() ?: return
    val activityIntent = if (!uri.isNullOrEmpty()) {
        Intent(Intent.ACTION_VIEW, Uri.parse(uri))
    } else {
        Intent(this, clazz)
    }
    if (intent != null && intent.extras != null) {
        activityIntent.putExtras(intent.extras!!)
    }
    try {
        startActivities(clazz, activityIntent)
    } catch (e: Exception) {
        e.printStackTrace()
    }
}

fun Context.getActivity(): Class<out Activity>? {
    val packageName = packageName
    val launchIntent = packageManager.getLaunchIntentForPackage(packageName) ?: return null
    val className = launchIntent.component!!.className
    var clazz: Class<out Activity>? = null
    try {
        clazz = Class.forName(className) as Class<out Activity>?
    } catch (e: ClassNotFoundException) {
        // do nothing
    }
    return clazz
}

fun Context.startActivities(clazz: Class<out Activity>?, activityIntent: Intent) {
    val stackBuilder = TaskStackBuilder.create(this)
    stackBuilder.addParentStack(clazz)
    stackBuilder.addNextIntent(activityIntent)
    stackBuilder.startActivities()
}

fun Context.clearNotification(message: Message?) {
    if (!message?.carouselContent.isNullOrEmpty()) {
        for (item in message?.carouselContent!!) {
            item.removeFileFromStorage()
        }
    }
    val manager = this.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager?
    manager?.cancel(message?.messageSource, message?.messageId!!)
}

fun Context.areNotificationsEnabled(): Boolean {
    return if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
        val manager =
            this.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        if (!manager.areNotificationsEnabled()) {
            return false
        }
        val channels = manager.notificationChannels
        for (channel in channels) {
            if (channel.importance == NotificationManager.IMPORTANCE_NONE) {
                return false
            }
        }
        true
    } else {
        NotificationManagerCompat.from(this).areNotificationsEnabled()
    }
}