package com.dengage.sdk.domain.rfm.model

import org.junit.Assert
import org.junit.Test

class RFMItemTest {

    @Test
    fun `RFMItem constructor test`() {
        val id = "id"
        val categoryId = "categoryId"
        val personalized = true
        val gender = RFMGender.FEMALE
        val sequence = 1

        val rfmItem = RFMItem(
            id = id,
            categoryId = categoryId,
            personalized = personalized,
            gender = gender,
            sequence = sequence
        )

        Assert.assertEquals(id, rfmItem.id)
        Assert.assertEquals(categoryId, rfmItem.categoryId)
        Assert.assertEquals(personalized, rfmItem.personalized)
        Assert.assertEquals(gender, rfmItem.gender)
        Assert.assertEquals(sequence, rfmItem.sequence)
    }
}