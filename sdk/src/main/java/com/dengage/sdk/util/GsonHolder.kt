package com.dengage.sdk.util

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.google.gson.reflect.TypeToken

object GsonHolder {

    val gson: Gson by lazy {
        GsonBuilder().create()
    }

    fun toJson(source: Any?): String {
        return gson.toJson(source)
    }

    inline fun <reified T : Any> fromJson(json: String?): T? {
        return try {
            val type = object : TypeToken<T>() {}.type
            return gson.fromJson<T>(json, type)
        } catch (ex: Exception) {
            ex.printStackTrace()
            null
        }
    }
}
