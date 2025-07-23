package com.dengage.sdk.domain.push.model

import android.os.Bundle
import androidx.core.text.isDigitsOnly
import com.dengage.sdk.util.GsonHolder
import com.google.gson.annotations.SerializedName
import com.google.gson.reflect.TypeToken
import java.io.Serializable

data class Message(
    @SerializedName("messageId") val messageId: Int = 0,
    @SerializedName("messageSource") val messageSource: String? = "",
    @SerializedName("transactionId") val transactionId: String? = "",
    @SerializedName("messageDetails") val messageDetails: String? = "",
    @SerializedName("mediaUrl") val mediaUrl: String? = "",
    @SerializedName("media") val media: List<Media>?,
    @SerializedName("targetUrl") val targetUrl: String? = "",
    @SerializedName("title") val title: String? = "",
    @SerializedName("subTitle") val subTitle: String? = "",
    @SerializedName("message") val message: String? = "",
    @SerializedName("badge") val badge: Boolean? = false,
    @SerializedName("badgeCount") val badgeCount: Int? = 0,
    @SerializedName("sound") val sound: String? = "",
    @SerializedName("dengageCampId") val campaignId: Int? = 0,
    @SerializedName("current") val current: Int? = -1,
    @SerializedName("source") val source: String? = "",
    @SerializedName("dengageCampName") val campaignName: String? = "",
    @SerializedName("dengageSendId") val sendId: Int? = 0,
    @SerializedName("notificationType") val notificationType: NotificationType = NotificationType.RICH,
    @SerializedName("customParams") val customParams: List<CustomParam>?,
    @SerializedName("carouselContent") val carouselContent: List<CarouselItem>?,
    @SerializedName("actionButtons") val actionButtons: List<ActionButton>?,
    @SerializedName("addToInbox") val addToInbox: Boolean? = false,
    @SerializedName("expireDate") val expireDate: String?,
    @SerializedName("appId") val appId: String?
) : Serializable {

    companion object {
        fun createFromIntent(bundle: Bundle): Message {
            val bundleMap: HashMap<String, String> = HashMap()
            for (key in bundle.keySet()) {
                val value = bundle[key]
                if (value != null) {
                    bundleMap[key] = value.toString()
                }
            }

            return createFromMap(bundleMap)
        }

        fun createFromMap(bundleMap: Map<String, String?>): Message {

            val notificationType = if (bundleMap["notificationType"].isNullOrEmpty().not()) {
                NotificationType.valueOf(bundleMap["notificationType"]!!)
            } else {
                NotificationType.RICH
            }

            val messageId = if (bundleMap["messageId"].isNullOrEmpty()
                    .not() && bundleMap["messageId"]!!.isDigitsOnly()
            ) {
                bundleMap["messageId"]!!.toInt()
            } else {
                0
            }

            val campaignId = if (bundleMap["dengageCampId"].isNullOrEmpty()
                    .not() && bundleMap["dengageCampId"]!!.isDigitsOnly()
            ) {
                bundleMap["dengageCampId"]!!.toInt()
            } else {
                0
            }

            val campaignName = if (bundleMap["dengageCampName"].isNullOrEmpty().not()) {
                bundleMap["dengageCampName"]!!
            } else {
                ""
            }

            val sendId = if (bundleMap["dengageSendId"].isNullOrEmpty()
                    .not() && bundleMap["dengageSendId"]!!.isDigitsOnly()
            ) {
                bundleMap["dengageSendId"]!!.toInt()
            } else {
                0
            }

            val messageSource = if (bundleMap["messageSource"].isNullOrEmpty().not()) {
                bundleMap["messageSource"]!!
            } else {
                ""
            }

            val messageDetails = if (bundleMap["messageDetails"].isNullOrEmpty().not()) {
                bundleMap["messageDetails"]!!
            } else {
                ""
            }

            var mediaUrl = if (bundleMap["mediaUrl"].isNullOrEmpty().not()) {
                bundleMap["mediaUrl"]!!
            } else {
                ""
            }

            val targetUrl = if (bundleMap["targetUrl"].isNullOrEmpty().not()) {
                bundleMap["targetUrl"]!!
            } else {
                ""
            }

            val title = if (bundleMap["title"].isNullOrEmpty().not()) {
                bundleMap["title"]!!
            } else {
                ""
            }

            val transactionId = if (bundleMap["transactionId"].isNullOrEmpty().not()) {
                bundleMap["transactionId"]!!
            } else {
                ""
            }

            val subTitle = if (bundleMap["subTitle"].isNullOrEmpty().not()) {
                bundleMap["subTitle"]!!
            } else {
                ""
            }

            val message = if (bundleMap["message"].isNullOrEmpty().not()) {
                bundleMap["message"]!!
            } else {
                ""
            }

            val badge = if (bundleMap["badge"].isNullOrEmpty().not()) {
                bundleMap["badge"]!!.toBoolean()
            } else {
                false
            }

            val badgeCount = if (bundleMap["badgeCount"].isNullOrEmpty()
                    .not() && bundleMap["badgeCount"]!!.isDigitsOnly()
            ) {
                bundleMap["badgeCount"]!!.toInt()
            } else {
                0
            }

            val sound = if (bundleMap["sound"].isNullOrEmpty().not()) {
                bundleMap["sound"]!!
            } else {
                ""
            }

            val customParams = if (bundleMap["customParams"].isNullOrEmpty().not()) {
                val type = object : TypeToken<List<CustomParam>>() {}.type
                GsonHolder.gson.fromJson<List<CustomParam>>(bundleMap["customParams"]!!, type)
            } else {
                null
            }

            val carouselContent = if (bundleMap["carouselContent"].isNullOrEmpty().not()) {
                val type = object : TypeToken<List<CarouselItem>>() {}.type
                GsonHolder.gson.fromJson<List<CarouselItem>>(bundleMap["carouselContent"]!!, type)
            } else {
                null
            }

            val actionButtons = if (bundleMap["actionButtons"].isNullOrEmpty().not()) {
                val type = object : TypeToken<List<ActionButton>>() {}.type
                GsonHolder.gson.fromJson<List<ActionButton>>(bundleMap["actionButtons"]!!, type)
            } else {
                null
            }

            val media = if (bundleMap["media"].isNullOrEmpty().not()) {
                val type = object : TypeToken<List<Media>>() {}.type
                GsonHolder.gson.fromJson<List<Media>>(bundleMap["media"]!!, type)
            } else {
                null
            }

            val addToInbox = if (bundleMap["addToInbox"].isNullOrEmpty().not()) {
                bundleMap["addToInbox"]!!.toBoolean()
            } else {
                false
            }

            val expireDate = if (bundleMap["expireDate"].isNullOrEmpty().not()) {
                bundleMap["expireDate"]!!
            } else {
                ""
            }

            val source = if (bundleMap["source"].isNullOrEmpty().not()) {
                bundleMap["source"]!!
            } else {
                ""
            }
            if (media != null && media.isNotEmpty() && media[0].url != null) {
                mediaUrl = media[0].url!!
            }
            var current = -1
            if (carouselContent != null) {
                if (bundleMap.contains("current")) {
                    current = if (bundleMap["current"].isNullOrEmpty()
                            .not() && bundleMap["current"]!!.isDigitsOnly()
                    ) {
                        bundleMap["current"]!!.toInt()
                    } else {
                        -1
                    }
                }
            }

            val appId = if (bundleMap["appId"].isNullOrEmpty().not()) {
                bundleMap["appId"]!!
            } else {
                ""
            }

            return Message(
                messageId = messageId,
                messageSource = messageSource,
                transactionId = transactionId,
                messageDetails = messageDetails,
                mediaUrl = mediaUrl,
                media = media,
                targetUrl = targetUrl,
                source = source,
                title = title,
                subTitle = subTitle,
                message = message,
                badge = badge,
                badgeCount = badgeCount,
                sound = sound,
                campaignId = campaignId,
                campaignName = campaignName,
                sendId = sendId,
                notificationType = notificationType,
                customParams = customParams,
                carouselContent = carouselContent,
                actionButtons = actionButtons,
                addToInbox = addToInbox,
                expireDate = expireDate, current = current, appId = appId
            )
        }
    }
}