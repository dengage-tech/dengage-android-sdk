package com.dengage.sdk.manager.rfm

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.rfm.model.RFMGender
import com.dengage.sdk.domain.rfm.model.RFMItem
import com.dengage.sdk.domain.rfm.model.RFMScore
import kotlin.math.sqrt

class RFMManager {

    fun saveRFMScores(scores: MutableList<RFMScore>?) {
        Prefs.rfmScores = scores
    }

    fun categoryView(categoryId: String) {
        var rfmScores = Prefs.rfmScores
        val foundCategoryScore = rfmScores?.firstOrNull { it.categoryId == categoryId }
        if (foundCategoryScore == null) {
            if (rfmScores == null) {
                rfmScores = mutableListOf()
            }
            rfmScores.add(
                RFMScore(
                    categoryId = categoryId,
                    score = 0.5
                )
            )
        } else {
            increaseRfmScore(foundCategoryScore)
        }
        Prefs.rfmScores = rfmScores
    }

    fun <T> sortRFMItems(
        rfmGender: RFMGender,
        rfmItems: MutableList<RFMItem>
    ): MutableList<T> {
        val scoreArray = Prefs.rfmScores ?: mutableListOf()

        val notPersonalized = rfmItems.filter { !it.personalized }.toMutableList()
        val sortedNotPersonalized = sortByRfmScores(scoreArray, notPersonalized)

        val personalized = rfmItems.filter { it.personalized }.toMutableList()
        val genderSelecteds = mutableListOf<RFMItem>()
        val removePersonalizedItemIds = mutableListOf<String>()
        personalized.forEach { rfmItem ->
            if (rfmItem.gender == rfmGender || rfmItem.gender == RFMGender.NEUTRAL) {
                genderSelecteds.add(rfmItem)
                removePersonalizedItemIds.add(rfmItem.id)
            }
        }
        removePersonalizedItemIds.forEach { itemId ->
            personalized.removeAll { it.id == itemId }
        }
        val sortedGenderSelecteds = sortByRfmScores(scoreArray, genderSelecteds)

        val sortedPersonalized = sortByRfmScores(scoreArray, personalized)

        val result = mutableListOf<RFMItem>()
        result.addAll(sortedNotPersonalized)
        result.addAll(sortedGenderSelecteds)
        result.addAll(sortedPersonalized)

        return result as MutableList<T>
    }

    private fun sortByRfmScores(scores: MutableList<RFMScore>, rfmItems: MutableList<RFMItem>): MutableList<RFMItem> {
        val items = rfmItems.toMutableList()
        val result = mutableListOf<RFMItem>()

        val removeItemIds = mutableListOf<String>()
        scores.forEach { rfmScore ->
            items.forEach { rfmItem ->
                if (rfmScore.categoryId == rfmItem.categoryId) {
                    result.add(rfmItem)
                    removeItemIds.add(rfmItem.id)
                }
            }
        }

        removeItemIds.forEach { itemId ->
            items.removeAll { it.id == itemId }
        }

        val sortedItems = items.sortedBy { it.sequence }
        result.addAll(sortedItems)
        return result
    }

    private fun increaseRfmScore(rfmScore: RFMScore) {
        rfmScore.score += (sqrt(rfmScore.score) * rfmScore.score) / 4
    }

}