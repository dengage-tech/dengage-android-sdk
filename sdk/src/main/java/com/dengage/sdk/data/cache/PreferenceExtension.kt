package com.dengage.sdk.data.cache

import android.annotation.SuppressLint
import android.content.SharedPreferences
import com.dengage.sdk.util.GsonHolder
import java.util.*

/**
 * Finds value on given key.
 *
 * [T] is the type of value
 *
 * @param key          The name of the preference.
 * @param defaultValue Optional default value - will take null for strings, false for bool and -1 for numeric values if [defaultValue] is not specified
 * @return The value associated with this key, defValue if there isn't any
 */
inline fun <reified T : Any> SharedPreferences.get(key: Any, defaultValue: T? = null): T? {
    val keyName = key.toString()
    return when (T::class) {
        String::class -> getString(keyName, defaultValue as? String) as T?
        Int::class -> getInt(keyName, defaultValue as? Int ?: -1) as T?
        Boolean::class -> getBoolean(keyName, defaultValue as? Boolean ?: false) as T?
        Float::class -> getFloat(keyName, defaultValue as? Float ?: -1f) as T?
        Long::class -> getLong(keyName, defaultValue as? Long ?: -1) as T?
        else -> getString(keyName, null)?.let {
            GsonHolder.fromJson(it)
        }
    }
}

/**
 * Puts a key value pair in shared prefs if doesn't exists, otherwise updates value on given [key].
 *
 * @param key   The name of the preference.
 * @param value The new set for the preference.
 */
fun SharedPreferences.set(key: Any, value: Any?, immediately: Boolean = false) {
    val keyName = key.toString()
    when (value) {
        is String? -> edit(immediately) { it.putString(keyName, value) }
        is Int -> edit(immediately) { it.putInt(keyName, value) }
        is Boolean -> edit(immediately) { it.putBoolean(keyName, value) }
        is Float -> edit(immediately) { it.putFloat(keyName, value) }
        is Long -> edit(immediately) { it.putLong(keyName, value) }
        is Date -> edit(immediately) { it.putLong(keyName, value.time) }
        else -> edit(immediately) { it.putString(keyName, GsonHolder.toJson(value)) }
    }
}


@SuppressLint("ApplySharedPref")
fun SharedPreferences.edit(
    immediately: Boolean = false,
    operation: (SharedPreferences.Editor) -> Unit
) {
    val editor = this.edit()
    operation(editor)

    when (immediately) {
        true -> editor.commit()
        else -> editor.apply()
    }
}