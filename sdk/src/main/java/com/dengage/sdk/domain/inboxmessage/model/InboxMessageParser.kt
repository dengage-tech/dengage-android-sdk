package com.dengage.sdk.domain.inboxmessage.model

import com.dengage.sdk.domain.push.model.CarouselItem
import com.dengage.sdk.domain.push.model.CustomParam
import org.json.JSONArray
import org.json.JSONObject


object InboxMessageParser {

    fun parseInboxResponse(responseString: String): MutableList<InboxMessage> {
        val inboxMessageList = mutableListOf<InboxMessage>()

        try {
            val mainJsonArray = JSONArray(responseString)
            for (i in 0 until mainJsonArray.length()) {
                val inboxMessageJsonObject = mainJsonArray.getJSONObject(i)
                val hasMessageJsonObject = inboxMessageJsonObject.has("message_json")
                var messageJsonObject: JSONObject? = null
                val androidCarouselItems = ArrayList<CarouselItem>()
                val customParametersList = ArrayList<CustomParam>()
                if (hasMessageJsonObject) {
                    messageJsonObject = JSONObject(inboxMessageJsonObject.getString("message_json"))
                    
                    // Parse carousel items
                    if (messageJsonObject.has("androidCarouselContent") && 
                        messageJsonObject.getJSONArray("androidCarouselContent").length() > 0) {
                        for (j in 0 until messageJsonObject.getJSONArray("androidCarouselContent")
                            .length()) {
                            val carouselContent =
                                messageJsonObject.getJSONArray("androidCarouselContent")
                                    .getJSONObject(j)


                            var idFromObject = ""
                            var titleFromObject = ""
                            var descriptionFromObject = ""
                            var mediaUrlFromObject = ""
                            var targetUrlFromObject = ""

                            if(carouselContent.has("id")) idFromObject = carouselContent.getString("id")
                            if(carouselContent.has("title")) titleFromObject = carouselContent.getString("title")
                            if(carouselContent.has("desc")) descriptionFromObject = carouselContent.getString("desc")
                            if(carouselContent.has("mediaUrl")) mediaUrlFromObject = carouselContent.getString("mediaUrl")
                            if(carouselContent.has("targetUrl")) targetUrlFromObject = carouselContent.getString("targetUrl")

                            val carouselItem = CarouselItem().apply {
                                id = idFromObject
                                title = titleFromObject
                                description = descriptionFromObject
                                mediaUrl = mediaUrlFromObject
                                targetUrl = targetUrlFromObject
                            }
                            androidCarouselItems.add(carouselItem)
                        }
                    }
                    
                    // Parse custom parameters
                    if (messageJsonObject.has("customParameters") && 
                        messageJsonObject.getJSONArray("customParameters").length() > 0) {
                        for (k in 0 until messageJsonObject.getJSONArray("customParameters")
                            .length()) {
                            val customParamObject =
                                messageJsonObject.getJSONArray("customParameters")
                                    .getJSONObject(k)

                            var keyFromObject = ""
                            var valueFromObject = ""

                            if(customParamObject.has("key")) keyFromObject = customParamObject.getString("key")
                            if(customParamObject.has("value")) valueFromObject = customParamObject.getString("value")

                            val customParam = CustomParam(
                                key = keyFromObject,
                                value = valueFromObject
                            )
                            customParametersList.add(customParam)
                        }
                    }

                }
                val id = inboxMessageJsonObject.getString("smsg_id")
                val isClicked = inboxMessageJsonObject.getBoolean("is_clicked")

                val title = messageJsonObject?.getString("title")
                val message = messageJsonObject?.getString("message")
                val mediaUrl =
                    messageJsonObject?.getString("androidMediaUrl") ?: messageJsonObject?.getString(
                        "mediaUrl")
                val androidMediaUrl = messageJsonObject?.getString("androidMediaUrl")
                val targetUrl = messageJsonObject?.getString("androidTargetUrl")
                    ?: messageJsonObject?.getString("targetUrl")
                val androidTargetUrl = messageJsonObject?.getString("androidTargetUrl")
                val receiveDate = messageJsonObject?.getString("receiveDate")
                val inboxMessageObject = InboxMessage(id = id, isClicked = isClicked, isDeleted = false,
                    InboxMessageData(title = title,
                        message = message,
                        mediaUrl = mediaUrl,
                        androidMediaUrl = androidMediaUrl,
                        targetUrl = targetUrl,
                        androidTargetUrl = androidTargetUrl,
                        receiveDate = receiveDate,
                        carouselItems = androidCarouselItems,
                        customParameters = customParametersList))
                inboxMessageList.add(inboxMessageObject)
            }


        } catch (e: Exception) {
            e.printStackTrace()
        } catch (e: Throwable) {
            e.printStackTrace()
        }
        return inboxMessageList
    }
}