package com.dengage.sdk.domain.inappmessage.model

import org.junit.Assert
import org.junit.Test

class ContentParamsTest {

    @Test
    fun `ContentParams constructor test`() {
        val position = ContentPosition.BOTTOM.position
        val shouldAnimate = true
        val html = "html"
        val maxWidth = 100
        val radius = 5
        val marginTop = 5
        val marginBottom = 5
        val marginLeft = 5
        val marginRight = 5
        val dismissOnTouchOutside = false

        val contentParams = ContentParams(
            position = position,
            shouldAnimate = shouldAnimate,
            html = html,
            maxWidth = maxWidth,
            radius = radius,
            marginTop = marginTop,
            marginBottom = marginBottom,
            marginLeft = marginLeft,
            marginRight = marginRight,
            dismissOnTouchOutside = dismissOnTouchOutside
        )

        Assert.assertEquals(position, contentParams.position)
        Assert.assertEquals(shouldAnimate, contentParams.shouldAnimate)
        Assert.assertEquals(html, contentParams.html)
        Assert.assertEquals(maxWidth, contentParams.maxWidth)
        Assert.assertEquals(radius, contentParams.radius)
        Assert.assertEquals(marginTop, contentParams.marginTop)
        Assert.assertEquals(marginBottom, contentParams.marginBottom)
        Assert.assertEquals(marginLeft, contentParams.marginLeft)
        Assert.assertEquals(marginRight, contentParams.marginRight)
        Assert.assertEquals(dismissOnTouchOutside, contentParams.dismissOnTouchOutside)
    }

}