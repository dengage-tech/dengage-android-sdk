package com.dengage.sdk.domain.inboxmessage.model

import com.dengage.sdk.util.GsonHolder
import com.google.gson.Gson
import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonParseException
import com.google.gson.JsonSerializationContext
import com.google.gson.JsonSerializer
import java.lang.reflect.Type

class InboxMessageDataJsonAdapter : JsonDeserializer<InboxMessageData>, JsonSerializer<InboxMessageData?> {

    @Throws(JsonParseException::class)
    override fun deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): InboxMessageData {
        val inboxMessageData = if (json.isJsonObject) {
            GsonHolder.gson.fromJson(json.asJsonObject, InboxMessageData::class.java)
        } else {
            val str: String = json.asJsonPrimitive.asString
            GsonHolder.gson.fromJson(str, InboxMessageData::class.java)
        }
        inboxMessageData.mediaUrl = inboxMessageData.androidMediaUrl ?: inboxMessageData.mediaUrl
        inboxMessageData.targetUrl = inboxMessageData.androidTargetUrl ?: inboxMessageData.targetUrl
        return inboxMessageData
    }

    override fun serialize(src: InboxMessageData?, typeOfSrc: Type, context: JsonSerializationContext): JsonElement {
        return context.serialize(src)
    }
}