package com.dengage.sdk.domain.push.model

import org.junit.Assert
import org.junit.Test

class CustomParamTest {

    @Test
    fun `CustomParam constructor test`() {
        val key = "key"
        val value = "value"

        val customParam = CustomParam(
            key = key,
            value = value
        )

        Assert.assertEquals(key, customParam.key)
        Assert.assertEquals(value, customParam.value)
    }

}