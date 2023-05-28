package com.dengage.sdk.domain.inboxmessage.model

import com.dengage.sdk.domain.push.model.CarouselItem
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
                if (hasMessageJsonObject) {
                    messageJsonObject = JSONObject(inboxMessageJsonObject.getString("message_json"))
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
                val inboxMessageObject = InboxMessage(id = id, isClicked = isClicked,
                    InboxMessageData(title = title,
                        message = message,
                        mediaUrl = mediaUrl,
                        androidMediaUrl = androidMediaUrl,
                        targetUrl = targetUrl,
                        androidTargetUrl = androidTargetUrl,
                        receiveDate = receiveDate,
                        carouselItems = androidCarouselItems))
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