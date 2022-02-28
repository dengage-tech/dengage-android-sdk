package com.dengage.sdk.util.extension

import android.content.Context
import android.graphics.Bitmap
import java.io.File
import java.io.FileOutputStream
import java.io.IOException
import java.lang.Exception

fun Bitmap.scale(): Bitmap {
    val requiredWidth = 250
    val requiredHeight = 250
    var divider = 1
    if (height > requiredHeight || width > requiredWidth) {
        val halfHeight = height / 2
        val halfWidth = width / 2
        while (halfHeight / divider >= requiredHeight
            && halfWidth / divider >= requiredWidth
        ) {
            divider *= 2
        }
    }

    return Bitmap.createScaledBitmap(
        this,
        width / divider,
        height / divider,
        false
    )
}

fun Bitmap.saveToInternalStorage(context: Context, fileName: String): String? {
    var fileSaved = false
    val directory = context.cacheDir
    val file = File(directory, "$fileName.png")
    var fileOutputStream: FileOutputStream? = null
    try {
        fileOutputStream = FileOutputStream(file)
        this.compress(Bitmap.CompressFormat.PNG, 80, fileOutputStream)
        fileSaved = true
    } catch (e: Exception) {
        e.printStackTrace()
    } finally {
        try {
            fileOutputStream?.close()
        } catch (e: IOException) {
            e.printStackTrace()
        }
    }
    return if (fileSaved) directory.absolutePath else null
}