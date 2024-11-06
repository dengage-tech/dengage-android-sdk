package com.dengage.sdk.util.extension

import android.app.Activity
import android.app.TaskStackBuilder
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.os.Build
import android.os.DeadObjectException
import android.provider.Settings
import android.text.TextUtils
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils

fun Context.launchActivity(intent: Intent?, uri: String?) {
    val clazz = this.getActivityClass() ?: return

    val activityIntent = if (uri != null && !TextUtils.isEmpty(uri)) {
        Intent(Intent.ACTION_VIEW, Uri.parse(uri))
    } else {
        Intent(this, clazz)
    }

    if (intent != null && intent.extras != null) {
        activityIntent.putExtras(intent.extras!!)
    }

    try {
        activityIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        activityIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP)
        this.startActivity(activityIntent)
    } catch (e: Exception) {
        DengageLogger.error(e.message)
    }
}

fun Context.getActivityClass(): Class<out Activity>? {
    val packageName = this.packageName
    val launchIntent = this.packageManager.getLaunchIntentForPackage(packageName) ?: return null
    val className = launchIntent.component?.className
    var clazz: Class<out Activity>? = null
    try {
        clazz = DengageUtils.getClassName(className)?.let { Class.forName(it) } as Class<out Activity>
    } catch (e: Exception) {
        DengageLogger.error(e.message)
    }
    catch (ex: Throwable)
    { ex.printStackTrace()

    }
    catch (e: DeadObjectException)
    {
        e.printStackTrace()

    }
    return clazz
}

fun Context.startActivityFromClass(clazz: Class<out Activity>?, activityIntent: Intent) {
    val stackBuilder = TaskStackBuilder.create(this)
    stackBuilder.addParentStack(clazz)
    stackBuilder.addNextIntent(activityIntent)
    stackBuilder.startActivities()
}

fun Context.launchNotificationSettingsActivity() {
    try {
        val intent = Intent()
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            intent.action = Settings.ACTION_APP_NOTIFICATION_SETTINGS
            intent.putExtra(Settings.EXTRA_APP_PACKAGE, this.packageName)
        } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            intent.action = "android.settings.APP_NOTIFICATION_SETTINGS"
            intent.putExtra("app_package", this.packageName)
            intent.putExtra("app_uid", this.applicationInfo.uid)
        } else {
            intent.action = Settings.ACTION_APPLICATION_DETAILS_SETTINGS
            intent.addCategory(Intent.CATEGORY_DEFAULT)
            intent.data = Uri.parse("package:$this.packageName")
        }

        this.startActivity(intent)
    }
    catch (ex: Exception)
    { ex.printStackTrace()

    }
    catch (ex: Throwable)
    { ex.printStackTrace()

    }
    catch (e: DeadObjectException)
    {
        e.printStackTrace()

    }
}

fun Context.launchApplicationSettingsActivity() {
    try {
        val intent = Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS).apply {
            data = Uri.parse("package:$packageName")
        }
        this.startActivity(intent)
    } catch (ex: Exception) {
        ex.printStackTrace()
    } catch (ex: Throwable) {
        ex.printStackTrace()
    } catch (e: DeadObjectException) {
        e.printStackTrace()
    }
}

