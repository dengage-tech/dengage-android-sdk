package com.dengage.sdk.domain.push.model

import org.junit.Assert
import org.junit.Test

class ActionButtonTest {

    @Test
    fun `ActionButton constructor test`() {
        val id = "id"
        val text = "text"
        val icon = "icon"
        val targetUrl = "targetUrl"

        val actionButton = ActionButton(
            id = id,
            text = text,
            icon = icon,
            targetUrl = targetUrl
        )

        Assert.assertEquals(id, actionButton.id)
        Assert.assertEquals(text, actionButton.text)
        Assert.assertEquals(icon, actionButton.icon)
        Assert.assertEquals(targetUrl, actionButton.targetUrl)
    }

}