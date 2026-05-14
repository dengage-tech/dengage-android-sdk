package com.dengage.sdk.ui.story

import android.content.Context
import android.util.AttributeSet
import android.view.GestureDetector
import android.view.MotionEvent
import androidx.core.view.GestureDetectorCompat
import androidx.viewpager.widget.ViewPager
import kotlin.math.abs

class FixedViewPager @JvmOverloads constructor(context: Context, attrs: AttributeSet? = null) :
    ViewPager(context, attrs) {

    /** Invoked when the user performs a horizontal fling past the first/last page. */
    var onEdgeSwipeOut: (() -> Unit)? = null

    private val edgeFlingDetector = GestureDetectorCompat(context, object : GestureDetector.SimpleOnGestureListener() {
        override fun onFling(
            e1: MotionEvent?,
            e2: MotionEvent,
            velocityX: Float,
            velocityY: Float
        ): Boolean {
            if (abs(velocityX) < FLING_VELOCITY_THRESHOLD) return false
            if (abs(velocityX) < abs(velocityY)) return false
            val count = adapter?.count ?: return false
            // Swipe-right gesture (positive velocityX) while at the first page → close.
            // Swipe-left gesture (negative velocityX) while at the last page → close.
            val atFirst = currentItem == 0 && velocityX > 0
            val atLast = currentItem == count - 1 && velocityX < 0
            if (atFirst || atLast) {
                onEdgeSwipeOut?.invoke()
                return true
            }
            return false
        }
    })

    override fun onInterceptTouchEvent(ev: MotionEvent): Boolean {
        return if (isFakeDragging) {
            false
        } else try {
            super.onInterceptTouchEvent(ev)
        } catch (e: IllegalArgumentException) {
            false
        }
    }

    override fun onTouchEvent(ev: MotionEvent): Boolean {
        edgeFlingDetector.onTouchEvent(ev)
        return super.onTouchEvent(ev)
    }

    companion object {
        // Roughly matches Android's default minimum fling velocity (px/s).
        private const val FLING_VELOCITY_THRESHOLD = 500f
    }
}