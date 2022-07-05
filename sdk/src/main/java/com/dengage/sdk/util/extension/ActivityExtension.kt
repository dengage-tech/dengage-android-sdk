package com.dengage.sdk.util.extension

import android.app.Activity
import android.app.TaskStackBuilder
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.text.TextUtils
import com.dengage.sdk.util.DengageLogger

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
        clazz = Class.forName(className!!) as Class<out Activity>
    } catch (e: Exception) {
        DengageLogger.error(e.message)
    }
    return clazz
}

fun Context.startActivityFromClass(clazz: Class<out Activity>?, activityIntent: Intent) {
    val stackBuilder = TaskStackBuilder.create(this)
    stackBuilder.addParentStack(clazz)
    stackBuilder.addNextIntent(activityIntent)
    stackBuilder.startActivities()
}
