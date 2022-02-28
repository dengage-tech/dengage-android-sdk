package com.dengage.sdk.data.remote.provider

import com.dengage.sdk.util.GsonHolder
import com.google.gson.annotations.SerializedName
import retrofit2.Converter
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import java.lang.reflect.Type

class ConverterFactoryProvider {
    val converterFactories: List<Converter.Factory>
        get() = arrayListOf(
            EnumConverterFactory(),
            GsonConverterFactory.create(GsonHolder.gson)
        )
}

class EnumConverterFactory : Converter.Factory() {

    override fun stringConverter(
        type: Type,
        annotations: Array<Annotation>,
        retrofit: Retrofit
    ): Converter<Enum<*>, String>? {
        return if (type is Class<*> && type.isEnum) {
            Converter { enum ->
                try {
                    enum.javaClass.getField(enum.name)
                        .getAnnotation(SerializedName::class.java)?.value
                } catch (exception: Exception) {
                    null
                } ?: enum.toString()
            }
        } else {
            null
        }
    }
}
