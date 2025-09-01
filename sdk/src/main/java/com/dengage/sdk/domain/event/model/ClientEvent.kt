package com.dengage.sdk.domain.event.model

import java.io.Serializable

data class ClientEvent(
    val tableName: String,
    val eventType: String,
    val key: String?,
    val eventDetails: Map<String, Any>,
    val timestamp: Long = System.currentTimeMillis()
) : Serializable
