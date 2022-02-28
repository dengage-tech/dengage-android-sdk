package com.dengage.sdk.util

import kotlin.reflect.KClass

inline fun <reified T : Any> createLazy() =
    LazyCreator(T::class)

open class LazyCreator<T : Any>(
    private val clazz: KClass<T>
) : Lazy<T> {
    private var cached: T? = null

    open val factory by lazy { LazyFactory<T>() }

    override val value: T
        get() {
            if (cached == null) {
                cached = factory.of(clazz.java)
            }
            return cached as T
        }

    override fun isInitialized() = cached != null
}

open class LazyFactory<T : Any> {

    private val map = hashMapOf<Class<out T>, T>()

    @Synchronized
    fun of(clazz: Class<T>): T {
        if (map.contains(clazz).not()) {
            val instance = create(clazz)
            map[clazz] = instance
        }

        @Suppress("UNCHECKED_CAST")
        return map[clazz] as T
    }

    open fun create(clazz: Class<T>): T {
        try {
            return clazz.newInstance()
        } catch (e: ReflectiveOperationException) {
            throw RuntimeException("Cannot create an instance of $clazz", e)
        }
    }
}
