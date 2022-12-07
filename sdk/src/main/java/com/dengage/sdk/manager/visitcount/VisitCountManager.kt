package com.dengage.sdk.manager.visitcount

import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.visitcount.model.VisitCountItem
import java.text.SimpleDateFormat
import java.util.*

object VisitCountManager {

    private val dateFormatWithOutHours = SimpleDateFormat("dd.MM.yyyy", Locale.ENGLISH)

    fun updateVisitCount() {
        val dateTimeWithOutHour = findDateTimeWithOutHour(System.currentTimeMillis())
        val visitCountItems = Prefs.visitCountItems

        val foundVisitCountItem = visitCountItems.firstOrNull {
            it.dateTime == dateTimeWithOutHour
        }
        if (foundVisitCountItem == null) {
            visitCountItems.add(
                VisitCountItem(
                    dateTime = dateTimeWithOutHour,
                    visitCount = 1
                )
            )
            if (visitCountItems.size == 61) {
                visitCountItems.removeFirst()
            }
        } else {
            foundVisitCountItem.visitCount++
        }

        Prefs.visitCountItems = visitCountItems
    }

    fun findVisitCountSinceDays(dayCount: Int): Int {
        val calendar = Calendar.getInstance()
        calendar.add(Calendar.DAY_OF_YEAR, -dayCount)
        val sinceDaysWithOutHour = findDateTimeWithOutHour(calendar.timeInMillis)

        val visitCountItems = Prefs.visitCountItems
        visitCountItems.filter {
            it.dateTime > sinceDaysWithOutHour
        }.let { sinceVisitCountItems ->
            return sinceVisitCountItems.sumOf {
                it.visitCount
            }
        }
    }

    private fun findDateTimeWithOutHour(
        timeInMillis: Long
    ): Long {
        val date = Date(timeInMillis)
        val formattedDate = dateFormatWithOutHours.format(date)
        return dateFormatWithOutHours.parse(formattedDate)!!.time
    }

}
