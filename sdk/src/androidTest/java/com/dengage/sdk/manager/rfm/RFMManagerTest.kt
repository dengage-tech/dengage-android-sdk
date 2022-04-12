package com.dengage.sdk.manager.rfm

import androidx.test.platform.app.InstrumentationRegistry
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.rfm.model.RFMGender
import com.dengage.sdk.domain.rfm.model.RFMItem
import com.dengage.sdk.domain.rfm.model.RFMScore
import com.dengage.sdk.util.ContextHolder
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.Test

class RFMManagerTest {

    private lateinit var rfmManager: RFMManager

    @Before
    fun setup() {
        ContextHolder.context = InstrumentationRegistry.getInstrumentation().context
        rfmManager = RFMManager()
    }

    @After
    fun tearDown() {
        Prefs.clear()
    }

    @Test
    fun saveRFMScoresTest() {
        val rfmScores = mutableListOf(
            RFMScore(
                categoryId = "1",
                score = 0.5
            ),
            RFMScore(
                categoryId = "2",
                score = 0.5
            ),
            RFMScore(
                categoryId = "3",
                score = 0.5
            )
        )
        rfmManager.saveRFMScores(
            scores = rfmScores
        )

        Assert.assertEquals(Prefs.rfmScores!!.size, 3)
    }

    @Test
    fun categoryViewContainsCategoryTest() {
        val viewCategoryId = "1"
        val rfmScores = mutableListOf(
            RFMScore(
                categoryId = "1",
                score = 0.5
            ),
            RFMScore(
                categoryId = "2",
                score = 0.5
            ),
            RFMScore(
                categoryId = "3",
                score = 0.5
            )
        )
        rfmManager.saveRFMScores(
            scores = rfmScores
        )

        rfmManager.categoryView(categoryId = viewCategoryId)

        val viewedCategoryScore = Prefs.rfmScores?.firstOrNull { it.categoryId == viewCategoryId }
        val notViewedCategoryScore = Prefs.rfmScores?.firstOrNull { it.categoryId == "2" }
        Assert.assertNotEquals(viewedCategoryScore?.score, 0.5)
        Assert.assertEquals(notViewedCategoryScore?.score, 0.5)
    }

    @Test
    fun categoryViewDoesNotContainCategoryTest() {
        val viewCategoryId = "4"
        val rfmScores = mutableListOf(
            RFMScore(
                categoryId = "1",
                score = 0.5
            ),
            RFMScore(
                categoryId = "2",
                score = 0.5
            ),
            RFMScore(
                categoryId = "3",
                score = 0.5
            )
        )
        rfmManager.saveRFMScores(
            scores = rfmScores
        )

        rfmManager.categoryView(categoryId = viewCategoryId)

        val viewedCategoryScore = Prefs.rfmScores?.firstOrNull { it.categoryId == viewCategoryId }
        Assert.assertEquals(viewedCategoryScore?.score, 0.5)
    }

    @Test
    fun categoryViewDoesNotContainCategoryAndScoreTest() {
        val viewCategoryId = "4"

        rfmManager.saveRFMScores(null)
        rfmManager.categoryView(categoryId = viewCategoryId)

        val viewedCategoryScore = Prefs.rfmScores?.firstOrNull { it.categoryId == viewCategoryId }
        Assert.assertEquals(viewedCategoryScore?.score, 0.5)
        Assert.assertEquals(Prefs.rfmScores!!.size, 1)
    }

    @Test
    fun sortRfmItemsNotPersonalizedSequenceTest() {
        val rfmScores = mutableListOf(
            RFMScore(
                categoryId = "2",
                score = 0.5
            ),
            RFMScore(
                categoryId = "3",
                score = 0.5
            ),
            RFMScore(
                categoryId = "4",
                score = 0.5
            )
        )
        rfmManager.saveRFMScores(
            scores = rfmScores
        )

        val rfmItems = mutableListOf(
            RFMItem(
                id = "1",
                categoryId = "1",
                personalized = false,
                gender = RFMGender.FEMALE,
                sequence = 3
            ),
            RFMItem(
                id = "2",
                categoryId = "1",
                personalized = false,
                gender = RFMGender.FEMALE,
                sequence = 2
            ),
            RFMItem(
                id = "3",
                categoryId = "1",
                personalized = false,
                gender = RFMGender.FEMALE,
                sequence = 1
            )
        )

        val sortedRfmItems = rfmManager.sortRFMItems<RFMItem>(
            rfmGender = RFMGender.FEMALE,
            rfmItems = rfmItems
        )

        Assert.assertEquals(sortedRfmItems[0].id, "3")
        Assert.assertEquals(sortedRfmItems[1].id, "2")
        Assert.assertEquals(sortedRfmItems[2].id, "1")
    }

    @Test
    fun sortRfmItemsPersonalizedAndNotPersonalizedSequenceTest() {
        val rfmScores = mutableListOf(
            RFMScore(
                categoryId = "2",
                score = 0.5
            ),
            RFMScore(
                categoryId = "3",
                score = 0.5
            ),
            RFMScore(
                categoryId = "4",
                score = 0.5
            )
        )
        rfmManager.saveRFMScores(
            scores = rfmScores
        )

        val rfmItems = mutableListOf(
            RFMItem(
                id = "1",
                categoryId = "1",
                personalized = true,
                gender = RFMGender.FEMALE,
                sequence = 3
            ),
            RFMItem(
                id = "2",
                categoryId = "1",
                personalized = false,
                gender = RFMGender.FEMALE,
                sequence = 2
            ),
            RFMItem(
                id = "3",
                categoryId = "1",
                personalized = true,
                gender = RFMGender.FEMALE,
                sequence = 1
            ),
            RFMItem(
                id = "4",
                categoryId = "1",
                personalized = false,
                gender = RFMGender.FEMALE,
                sequence = 1
            )
        )

        val sortedRfmItems = rfmManager.sortRFMItems<RFMItem>(
            rfmGender = RFMGender.FEMALE,
            rfmItems = rfmItems
        )

        Assert.assertEquals(sortedRfmItems[0].id, "4")
        Assert.assertEquals(sortedRfmItems[1].id, "2")
        Assert.assertEquals(sortedRfmItems[2].id, "3")
        Assert.assertEquals(sortedRfmItems[3].id, "1")
    }

    @Test
    fun sortRfmItemsGenderTest() {
        val rfmScores = mutableListOf(
            RFMScore(
                categoryId = "2",
                score = 0.5
            ),
            RFMScore(
                categoryId = "3",
                score = 0.5
            ),
            RFMScore(
                categoryId = "4",
                score = 0.5
            )
        )
        rfmManager.saveRFMScores(
            scores = rfmScores
        )

        val rfmItems = mutableListOf(
            RFMItem(
                id = "1",
                categoryId = "1",
                personalized = true,
                gender = RFMGender.MALE,
                sequence = 3
            ),
            RFMItem(
                id = "2",
                categoryId = "1",
                personalized = false,
                gender = RFMGender.FEMALE,
                sequence = 2
            ),
            RFMItem(
                id = "3",
                categoryId = "1",
                personalized = true,
                gender = RFMGender.FEMALE,
                sequence = 1
            ),
            RFMItem(
                id = "4",
                categoryId = "1",
                personalized = false,
                gender = RFMGender.MALE,
                sequence = 1
            )
        )

        val sortedRfmItems = rfmManager.sortRFMItems<RFMItem>(
            rfmGender = RFMGender.MALE,
            rfmItems = rfmItems
        )

        Assert.assertEquals(sortedRfmItems[0].id, "4")
        Assert.assertEquals(sortedRfmItems[1].id, "2")
        Assert.assertEquals(sortedRfmItems[2].id, "1")
        Assert.assertEquals(sortedRfmItems[3].id, "3")
    }

}