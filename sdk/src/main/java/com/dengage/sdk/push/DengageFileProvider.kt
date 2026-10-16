package com.dengage.sdk.push

import androidx.core.content.FileProvider

/**
 * Shares push images with the system UI, e.g. the image of a conversation notification.
 * A subclass so that it does not clash with a FileProvider declared by the app.
 */
class DengageFileProvider : FileProvider()
