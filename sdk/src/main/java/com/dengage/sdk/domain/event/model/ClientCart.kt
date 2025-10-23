package com.dengage.sdk.domain.event.model

import java.io.Serializable

data class ClientCart(
    val items: Map<String, Any>,
    val timestamp: Long = System.currentTimeMillis()
) : Serializable