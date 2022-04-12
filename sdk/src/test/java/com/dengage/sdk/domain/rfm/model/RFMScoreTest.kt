package com.dengage.sdk.domain.rfm.model

import org.junit.Assert
import org.junit.Test

class RFMScoreTest {

    @Test
    fun `RFMScore constructor test`() {
        val categoryId = "categoryId"
        val score = 0.5

        val rfmScore = RFMScore(
            categoryId = categoryId,
            score = score
        )

        Assert.assertEquals(categoryId, rfmScore.categoryId)
        Assert.assertEquals(score, rfmScore.score)
    }
}