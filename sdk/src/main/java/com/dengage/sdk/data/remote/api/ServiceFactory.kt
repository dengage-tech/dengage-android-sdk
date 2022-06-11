package com.dengage.sdk.data.remote.api

import kotlin.reflect.KClass

object ServiceFactory {
    private val serviceMap = hashMapOf<Class<*>, Any>()

    @Synchronized
    fun <Service : Any> of(
        serviceClass: Class<Service>,
        apiType: ApiType
    ): Service {
        if (serviceMap.contains(serviceClass).not()) {
            val service = create(serviceClass, apiType)
            serviceMap[serviceClass] = service
        }

        @Suppress("UNCHECKED_CAST")
        return serviceMap[serviceClass] as Service
    }

    private fun <Service : Any> create(
        serviceClass: Class<Service>,
        apiType: ApiType
    ): Service {
        return when (apiType) {
            ApiType.EVENT -> {
                EventApiProvider.INSTANCE.create(serviceClass)
            }
            ApiType.PUSH -> {
                PushApiProvider.INSTANCE.create(serviceClass)
            }
            else -> {
                GeofenceApiProvider.INSTANCE.create(serviceClass)
            }
        }
    }
}

inline fun <reified Service : Any> service(
    apiType: ApiType
) = LazyRepositoryCreator(Service::class, apiType)

class LazyRepositoryCreator<Service : Any>(
    private val serviceClass: KClass<Service>,
    private val apiType: ApiType
) : Lazy<Service> {
    private var cached: Service? = null

    override val value: Service
        get() {
            if (cached == null) {
                cached = ServiceFactory.of(serviceClass.java, apiType)
            }
            return cached as Service
        }

    override fun isInitialized() = cached != null
}