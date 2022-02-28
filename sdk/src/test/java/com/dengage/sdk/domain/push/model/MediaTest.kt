package com.dengage.sdk.domain.push.model

import org.junit.Assert
import org.junit.Test

class MediaTest {

    @Test
    fun `Media constructor test`() {
        val url = "url"
        val target = "target"

        val media = Media(
            url = url,
            target = target
        )

        Assert.assertEquals(url, media.url)
        Assert.assertEquals(target, media.target)
    }

}