package com.dengage.sdk.domain.inappmessage.model

import org.junit.Assert
import org.junit.Test

class ContentTest {

    @Test
    fun `Content constructor test`() {
        val contentType = ContentType.HTML.type
        val targetUrl = "targetUrl"
        val contentParams = ContentParams(
            position = ContentPosition.BOTTOM.position,
            shouldAnimate = true,
            html = null,
            maxWidth = null,
            radius = null,
            marginTop = null,
            marginBottom = null,
            marginLeft = null,
            marginRight = null,
            dismissOnTouchOutside = false
        )
        val content = Content(
            type = contentType,
            targetUrl = targetUrl,
            params = contentParams
        )
        Assert.assertEquals(contentType, content.type)
        Assert.assertEquals(targetUrl, content.targetUrl)
        Assert.assertEquals(contentParams, content.params)
    }

}