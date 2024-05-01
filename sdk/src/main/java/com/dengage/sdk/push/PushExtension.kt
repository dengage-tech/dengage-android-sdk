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
import android.os.DeadObjectException
import android.text.TextUtils
import androidx.core.app.NotificationManagerCompat
import androidx.core.text.isDigitsOnly
import com.dengage.sdk.Dengage
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.push.model.CarouselItem
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import java.io.File
import java.io.FileInputStream

fun String?.getSoundUri(context: Context): Uri {
    try {
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
    } catch (e: Exception) {
    } catch (e: Throwable) {
    }
    return Uri.parse("")
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
    } catch (ex: Throwable) {
        ex.printStackTrace()
        -1
    } catch (e: DeadObjectException) {
        e.printStackTrace()
        -1
    }
}

fun Context.getSmallIconColorId(): Int {
    try {
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
    } catch (e: Exception) {
    } catch (e: Throwable) {
    }
    return -1
}

fun Context.getColorResourceId(resourceName: String?): Int {
    try {
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
    } catch (e: Exception) {
    } catch (e: Throwable) {
    }
    return -1
}

fun Context.getResourceId(resourceName: String?): Int {
    try {
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
    } catch (e: Exception) {
    } catch (e: Throwable) {
    }
    return -1
}

fun CarouselItem.removeFileFromStorage(): Boolean {
    try {
        val file = File(mediaFileLocation, "$mediaFileName.png")
        return if (file.exists()) {
            file.delete()
        } else {
            false
        }
    } catch (e: Exception) {
    } catch (e: Throwable) {
    }
    return false
}

fun CarouselItem.loadFileFromStorage(): Bitmap? {
    try {
        val file = File(mediaFileLocation, "$mediaFileName.png")
        return if (file.exists()) {
            BitmapFactory.decodeStream(FileInputStream(file))
        } else {
            null
        }
    } catch (e: Exception) {
    } catch (e: Throwable) {
    }
    return null
}

fun Context.launchActivity(intent: Intent?, uri: String?) {
    try {
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
    } catch (e: Exception) {
    } catch (e: Throwable) {
    }
}

fun Context.getActivity(): Class<out Activity>? {
    var clazz: Class<out Activity>? = null

    try {
        if (!DengageUtils.restartApplication()) {
            return Dengage.getCurrentActivity()?.javaClass
        }
        val packageName = packageName
        val launchIntent = packageManager.getLaunchIntentForPackage(packageName) ?: return null
        val className = launchIntent.component?.className

        clazz = DengageUtils.getClassName(className)?.let { Class.forName(it) } as Class<out Activity>?
    } catch (e: ClassNotFoundException) {
        // do nothing
    } catch (ex: Throwable) {
        ex.printStackTrace()

    } catch (e: DeadObjectException) {
        e.printStackTrace()

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
    try {
        if (!message?.carouselContent.isNullOrEmpty()) {
            for (item in message?.carouselContent!!) {
                item.removeFileFromStorage()
            }
        }
        val manager = this.getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager?
        manager?.cancel(message?.messageSource, message?.messageId!!)
        for (nid in Constants.listOfNotificationIds) { manager?.cancel(Integer.parseInt(nid.toString())) }

    } catch (ex: Exception) {

    } catch (ex: Throwable) {

    }
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
