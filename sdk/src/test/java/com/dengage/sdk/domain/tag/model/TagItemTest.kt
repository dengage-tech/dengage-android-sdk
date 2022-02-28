package com.dengage.sdk.domain.tag.model

import org.junit.Assert
import org.junit.Test

class TagItemTest {

    @Test
    fun `TagItem constructor test`() {
        val tag = "tag"
        val value = "value"

        val tagItem = TagItem(
            tag = tag,
            value = value
        )

        Assert.assertEquals(tag, tagItem.tag)
        Assert.assertEquals(value, tagItem.value)
    }

}