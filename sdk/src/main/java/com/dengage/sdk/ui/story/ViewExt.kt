package com.dengage.sdk.ui.story

import android.view.View
import android.content.Context
import android.util.TypedValue

fun View.hide() {
    visibility = View.GONE
}

fun View.show() {
    visibility = View.VISIBLE
}

fun pxToDp(context: Context, px: Float): Float {
    return TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, px, context.resources.displayMetrics)
}
