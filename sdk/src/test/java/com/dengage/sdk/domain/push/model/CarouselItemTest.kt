package com.dengage.sdk.domain.push.model

import org.junit.Assert
import org.junit.Test

class CarouselItemTest {

    @Test
    fun `CarouselItem constructor test`() {
        val id = "id"
        val title = "title"
        val description = "description"
        val mediaUrl = "mediaUrl"
        val targetUrl = "targetUrl"
        val type = "type"
        val mediaFileLocation = "mediaFileLocation"
        val mediaFileName = "mediaFileName"

        val carouselItem = CarouselItem(
            id = id,
            title = title,
            description = description,
            mediaUrl = mediaUrl,
            targetUrl = targetUrl,
            type = type,
            mediaFileLocation = mediaFileLocation,
            mediaFileName = mediaFileName,
        )

        Assert.assertEquals(id, carouselItem.id)
        Assert.assertEquals(title, carouselItem.title)
        Assert.assertEquals(description, carouselItem.description)
        Assert.assertEquals(mediaUrl, carouselItem.mediaUrl)
        Assert.assertEquals(targetUrl, carouselItem.targetUrl)
        Assert.assertEquals(type, carouselItem.type)
        Assert.assertEquals(mediaFileLocation, carouselItem.mediaFileLocation)
        Assert.assertEquals(mediaFileName, carouselItem.mediaFileName)
    }

}