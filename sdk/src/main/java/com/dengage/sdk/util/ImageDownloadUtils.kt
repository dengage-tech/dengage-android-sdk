package com.dengage.sdk.util

import android.content.Context
import android.graphics.Bitmap
import android.graphics.BitmapFactory
import com.dengage.sdk.util.extension.saveToInternalStorage
import com.dengage.sdk.util.extension.scale
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import okhttp3.OkHttpClient
import okhttp3.Request

object ImageDownloadUtils {

    private const val TAG = "ImageDownload"
    private var scope: CoroutineScope = CoroutineScope(Dispatchers.IO)

    fun downloadImages(
        context: Context,
        imageUrls: List<String>?,
        onComplete: ((MutableList<String>, MutableList<String>) -> Unit)?
    ) {
        val imageFilePaths = mutableListOf<String>()
        val imageFileNames = mutableListOf<String>()

        if (imageUrls.isNullOrEmpty()) {
            DengageLogger.error("$TAG given imageUrls are empty")
            onComplete?.invoke(imageFileNames, imageFilePaths)
            return
        }
        scope.launch {

            for (imageUrl in imageUrls) {

                val time = System.currentTimeMillis()

                // get bitmap from media url
                val bitmap = try {
                    getBitmapFromUrl(imageUrl)
                } catch (e: Exception) {
                    DengageLogger.error("$TAG ${e.message}")
                    null
                }

                // scale bitmap with small size
                val scaledBitmap = if (bitmap == null) {
                    DengageLogger.error("$TAG could not get bitmap from $imageUrl")
                    null
                } else {
                    try {
                        bitmap.scale()
                    } catch (e: Exception) {
                        DengageLogger.error("$TAG ${e.message}")
                        null
                    }
                }

                val imageFileName = "CarousalImage$time"

                // save image file to cache
                val imageFilePath = if (scaledBitmap == null) {
                    DengageLogger.error("$TAG could not scale bitmap")
                    null
                } else {
                    try {
                        scaledBitmap.saveToInternalStorage(
                            context = context,
                            fileName = imageFileName
                        )
                    } catch (e: Exception) {
                        DengageLogger.error("$TAG ${e.message}")
                        null
                    }
                }

                imageFilePaths.add(imageFilePath ?: "")
                imageFileNames.add(imageFileName)
            }

            onComplete?.invoke(imageFileNames, imageFilePaths)
        }
    }

    fun downloadImage(
        imageUrl: String?,
        onComplete: ((Bitmap?) -> Unit)?
    ) {
        if (imageUrl.isNullOrEmpty()) {
            DengageLogger.error("$TAG given imageUrl is empty")
            onComplete?.invoke(null)
            return
        }
        scope.launch {

            // get bitmap from media url
            val bitmap = try {
                getBitmapFromUrl(imageUrl)
            } catch (e: Exception) {
                DengageLogger.error("$TAG ${e.message}")
                DengageLogger.error("$TAG could not get bitmap from $imageUrl")
                null
            }

            onComplete?.invoke(bitmap)
        }
    }

    private fun getBitmapFromUrl(imageUrl: String): Bitmap? {
        val client = OkHttpClient()
        val request = Request.Builder().url(imageUrl).build()
        val response = client.newCall(request).execute()
        return BitmapFactory.decodeStream(response.body?.byteStream())
    }

}