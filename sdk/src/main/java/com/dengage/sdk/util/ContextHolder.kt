package com.dengage.sdk.util

import android.content.Context
import java.lang.ref.WeakReference

object ContextHolder {

    private lateinit var _context: WeakReference<Context>

    var context: Context
        get() = _context.get()!!
        set(value) {
            _context = WeakReference(value)
        }
}
