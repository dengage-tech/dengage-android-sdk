package com.dengage.sdk.ui.test

import android.app.Activity
import android.os.Bundle
import android.view.View
import android.widget.Button
import com.dengage.sdk.Dengage
import com.dengage.sdk.R
import com.dengage.sdk.util.EdgeToEdgeUtils
import com.dengage.sdk.domain.push.model.ActionButton
import com.dengage.sdk.domain.push.model.CarouselItem
import com.dengage.sdk.domain.push.model.CustomParam
import com.dengage.sdk.domain.push.model.Media
import com.dengage.sdk.domain.push.model.Message
import com.dengage.sdk.util.GsonHolder
import com.google.gson.reflect.TypeToken

class DengageTestPushActivity : Activity(), View.OnClickListener {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        // Enable edge-to-edge display for Android 15
        EdgeToEdgeUtils.enableEdgeToEdge(this)

        setContentView(R.layout.activity_dengage_test_push)

        findViewById<Button>(R.id.btnCarousel).setOnClickListener(this)
        findViewById<Button>(R.id.btnRich).setOnClickListener(this)
        findViewById<Button>(R.id.btnNormal).setOnClickListener(this)
    }

    override fun onClick(v: View?) {
        val message = when (v?.id) {
            R.id.btnCarousel -> {
                generateCarouselMessage()
            }
            R.id.btnRich -> {
                generateRichMessage()
            }
            R.id.btnNormal -> {
                generateNormalMessage()
            }
            else -> null
        }
        message?.let {
            val bundleMap = mutableMapOf<String, String?>()
            bundleMap["notificationType"] = message.notificationType.type
            bundleMap["messageId"] = message.messageId.toString()
            bundleMap["dengageCampId"] = message.campaignId.toString()
            bundleMap["dengageCampName"] = message.campaignName
            bundleMap["dengageSendId"] = message.sendId.toString()
            bundleMap["messageSource"] = message.messageSource
            bundleMap["messageDetails"] = message.messageDetails
            bundleMap["mediaUrl"] = message.mediaUrl
            bundleMap["targetUrl"] = message.targetUrl
            bundleMap["title"] = message.title
            bundleMap["transactionId"] = message.transactionId
            bundleMap["subTitle"] = message.subTitle
            bundleMap["message"] = message.message
            bundleMap["badge"] = (message.badge ?: false).toString()
            bundleMap["badgeCount"] = message.badgeCount.toString()
            bundleMap["sound"] = message.sound
            bundleMap["customParams"] = GsonHolder.gson.toJson(message.customParams, object : TypeToken<List<CustomParam?>>() {}.type)
            bundleMap["carouselContent"] = GsonHolder.gson.toJson(message.carouselContent, object : TypeToken<List<CarouselItem?>>() {}.type)
            bundleMap["actionButtons"] = GsonHolder.gson.toJson(message.actionButtons, object : TypeToken<List<ActionButton?>>() {}.type)
            bundleMap["media"] = GsonHolder.gson.toJson(message.media, object : TypeToken<List<Media?>>() {}.type)
            bundleMap["addToInbox"] = (message.addToInbox ?: false).toString()
            bundleMap["expireDate"] = message.expireDate

            Dengage.onMessageReceived(bundleMap)
        }
    }

    private fun generateCarouselMessage(): Message {
        return GsonHolder.gson.fromJson(
            "{\n" +
                    "    \"badge\": false,\n" +
                    "    \"messageId\": \"${(0..10000).random()}\",\n" +
                    "    \"sound\": \"\",\n" +
                    "    \"enabled\": true,\n" +
                    "    \"mediaUrl\": \"\",\n" +
                    "    \"messageSource\": \"DENGAGE\",\n" +
                    "    \"subTitle\": \"Test Subtitle\",\n" +
                    "    \"title\": \"Test Title\",\n" +
                    "    \"targetUrl\": \"\",\n" +
                    "    \"badgeCount\": 0,\n" +
                    "    \"codeSnippet\": null,\n" +
                    "    \"actionButtons\": [],\n" +
                    "    \"carouselContent\": [\n" +
                    "        {\n" +
                    "            \"desc\": \"\",\n" +
                    "            \"title\": \"Carousel 1\",\n" +
                    "            \"mediaUrl\": \"https://thumbs.dreamstime.com/b/financial-growth-graph--sales-increase-marketing-strategy-concept-abstract-cover-design-vertical-format-financial-growth-graph-130410421.jpg\",\n" +
                    "                 \"targetUrl\": \"https://www.google.com" +
        "        },\n" +
                "        {\n" +
                "            \"desc\": \"\",\n" +
                "            \"title\": \"Carousel 2\",\n" +
                "            \"mediaUrl\": \"https://thumbs.dreamstime.com/b/financial-growth-graph--sales-increase-marketing-strategy-concept-abstract-cover-design-vertical-format-financial-growth-graph-130410421.jpg\",\n" +
                "             \"targetUrl\": \"https://www.google.com" +
        "        },\n" +
                "        {\n" +
                "            \"desc\": \"\",\n" +
                "            \"title\": \"Carousel 3\",\n" +
                "            \"mediaUrl\": \"https://thumbs.dreamstime.com/b/financial-growth-graph--sales-increase-marketing-strategy-concept-abstract-cover-design-vertical-format-financial-growth-graph-130410421.jpg\",\n"+
                "             \"targetUrl\": \"https://www.google.com" +
                "        }\n" +
                "    ],\n" +
                "    \"actionButtonType\": \"NO_ACTION\",\n" +
                "    \"notificationType\": \"CAROUSEL\",\n" +
                "    \"carouselContentType\": \"ARRAY\",\n" +
                "    \"message\": \"Hi\",\n" +
                "\"customParams\": [\n" +
                "    {\n" +
                "        \"key\": \"target\",\n" +
                "        \"value\": \"https://www.google.com" +
                "    }\n" +
                "]\n" +
                "}", Message::class.java
        )
    }

    private fun generateRichMessage(): Message {
        return GsonHolder.gson.fromJson(
            "{\n" +
                    "    \"badge\": false,\n" +
                    "    \"messageId\": \"${(0..10000).random()}\",\n" +
                    "    \"sound\": \"\",\n" +
                    "    \"enabled\": true,\n" +
                    "    \"mediaUrl\": \"https://thumbs.dreamstime.com/b/financial-growth-graph--sales-increase-marketing-strategy-concept-abstract-cover-design-vertical-format-financial-growth-graph-130410421.jpg\",\n" +
                    "    \"messageSource\": \"DENGAGE\",\n" +
                    "    \"subTitle\": \"Test Subtitle\",\n" +
                    "    \"title\": \"Test Title\",\n" +
                    "    \"targetUrl\": \"\",\n" +
                    "    \"badgeCount\": 0,\n" +
                    "    \"codeSnippet\": null,\n" +
                    "    \"notificationType\": \"RICH\",\n" +
                    "    \"message\": \"Hi\",\n" +
                    "\"customParams\": [\n" +
                    "    {\n" +
                    "        \"key\": \"target\",\n" +
                    "        \"value\": \"https://thumbs.dreamstime.com/b/financial-growth-graph--sales-increase-marketing-strategy-concept-abstract-cover-design-vertical-format-financial-growth-graph-130410421.jpg\"\n" +
                    "    }\n" +
                    "]\n" +
                    "}", Message::class.java
        )
    }

    private fun generateNormalMessage(): Message {
        return GsonHolder.gson.fromJson(
            "{\n" +
                    "    \"badge\": false,\n" +
                    "    \"messageId\": \"${(0..10000).random()}\",\n" +
                    "    \"sound\": \"\",\n" +
                    "    \"enabled\": true,\n" +
                    "    \"mediaUrl\": \"https://thumbs.dreamstime.com/b/financial-growth-graph--sales-increase-marketing-strategy-concept-abstract-cover-design-vertical-format-financial-growth-graph-130410421.jpg\",\n" +
                    "    \"messageSource\": \"DENGAGE\",\n" +
                    "    \"subTitle\": \"Test Subtitle\",\n" +
                    "    \"title\": \"Test Title\",\n" +
                    "    \"targetUrl\": \"\",\n" +
                    "    \"badgeCount\": 0,\n" +
                    "    \"codeSnippet\": null,\n" +
                    "    \"notificationType\": \"TEXT\",\n" +
                    "    \"message\": \"https://thumbs.dreamstime.com/b/financial-growth-graph--sales-increase-marketing-strategy-concept-abstract-cover-design-vertical-format-financial-growth-graph-130410421.jpg\",\n" +
                    "\"customParams\": [\n" +
                    "    {\n" +
                    "        \"key\": \"target\",\n" +
                    "        \"value\": \"https://thumbs.dreamstime.com/b/financial-growth-graph--sales-increase-marketing-strategy-concept-abstract-cover-design-vertical-format-financial-growth-graph-130410421.jpg\"\n" +
                    "    }\n" +
                    "]\n" +
                    "}", Message::class.java
        )
    }
}
