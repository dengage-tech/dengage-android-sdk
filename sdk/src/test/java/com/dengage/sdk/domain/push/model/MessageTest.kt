package com.dengage.sdk.domain.push.model

import org.junit.Assert
import org.junit.Test

class MessageTest {

    @Test
    fun `Message constructor test`() {
        val messageId = 1
        val messageSource = "messageSource"
        val transactionId = "transactionId"
        val messageDetails = "messageDetails"
        val mediaUrl = "mediaUrl"
        val media = null
        val targetUrl = "targetUrl"
        val title = "title"
        val subTitle = "subTitle"
        val message = "message"
        val badge = true
        val badgeCount = 2
        val sound = "sound"
        val campaignId = 1
        val campaignName = "campaignName"
        val sendId = 2
        val notificationType = NotificationType.RICH
        val customParams = null
        val carouselContent = null
        val actionButtons = null
        val addToInbox = true
        val expireDate = "expireDate"

        val pushMessage = Message(
            messageId = messageId,
            messageSource = messageSource,
            transactionId = transactionId,
            messageDetails = messageDetails,
            mediaUrl = mediaUrl,
            media = media,
            targetUrl = targetUrl,
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
            expireDate = expireDate
        )

        Assert.assertEquals(messageId, pushMessage.messageId)
        Assert.assertEquals(messageSource, pushMessage.messageSource)
        Assert.assertEquals(transactionId, pushMessage.transactionId)
        Assert.assertEquals(messageDetails, pushMessage.messageDetails)
        Assert.assertEquals(mediaUrl, pushMessage.mediaUrl)
        Assert.assertEquals(media, pushMessage.media)
        Assert.assertEquals(targetUrl, pushMessage.targetUrl)
        Assert.assertEquals(title, pushMessage.title)
        Assert.assertEquals(subTitle, pushMessage.subTitle)
        Assert.assertEquals(message, pushMessage.message)
        Assert.assertEquals(badge, pushMessage.badge)
        Assert.assertEquals(badgeCount, pushMessage.badgeCount)
        Assert.assertEquals(sound, pushMessage.sound)
        Assert.assertEquals(campaignId, pushMessage.campaignId)
        Assert.assertEquals(campaignName, pushMessage.campaignName)
        Assert.assertEquals(sendId, pushMessage.sendId)
        Assert.assertEquals(notificationType, pushMessage.notificationType)
        Assert.assertEquals(customParams, pushMessage.customParams)
        Assert.assertEquals(carouselContent, pushMessage.carouselContent)
        Assert.assertEquals(actionButtons, pushMessage.actionButtons)
        Assert.assertEquals(addToInbox, pushMessage.addToInbox)
        Assert.assertEquals(expireDate, pushMessage.expireDate)
    }

}