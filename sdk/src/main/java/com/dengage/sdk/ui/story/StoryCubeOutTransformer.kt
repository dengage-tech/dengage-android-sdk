package com.dengage.sdk.ui.story

import android.view.View
import androidx.viewpager.widget.ViewPager

class StoryCubeOutTransformer @JvmOverloads constructor(
    private val distanceMultiplier: Int = 20
) : ViewPager.PageTransformer {

    override fun transformPage(page: View, position: Float) {
        page.apply {
            val clampedPosition = position.clamp(-1f, 1f)
            resetTransformations()
            if (clampedPosition in -1f..1f) {
                applyTransformations(clampedPosition)
            }
        }
    }

    private fun View.resetTransformations() {
        rotationX = 0f
        rotationY = 0f
        rotation = 0f
        scaleX = 1f
        scaleY = 1f
        pivotX = 0f
        pivotY = 0f
        translationY = 0f
        translationX = 0f
        alpha = 1f
        isEnabled = false
    }

    private fun View.applyTransformations(position: Float) {
        cameraDistance = (width * distanceMultiplier).toFloat()
        pivotX = if (position < 0f) width.toFloat() else 0f
        pivotY = height * 0.5f
        rotationY = 90f * position
        alpha = if (position <= -1f || position >= 1f) 0f else 1f
    }

    private fun Float.clamp(min: Float, max: Float): Float = when {
        this < min -> min
        this > max -> max
        else -> this
    }
}
