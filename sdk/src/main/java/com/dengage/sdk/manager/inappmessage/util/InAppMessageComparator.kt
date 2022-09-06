package com.dengage.sdk.manager.inappmessage.util

import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.util.Constants
import java.text.SimpleDateFormat
import java.util.*

class InAppMessageComparator : Comparator<InAppMessage> {
    override fun compare(first: InAppMessage, second: InAppMessage): Int {
        try {
            if (first.data.isRealTime() != second.data.isRealTime()) {
                return if (first.data.isRealTime() && !second.data.isRealTime()) {
                    1
                } else {
                    -1
                }
            } else {
                if (first.data.priority == second.data.priority) {
                    val simpleDateFormat = SimpleDateFormat(Constants.DATE_FORMAT, Locale.getDefault())
                    val firstExpireDate = simpleDateFormat.parse(first.data.expireDate)
                    val secondExpireDate = simpleDateFormat.parse(second.data.expireDate)
                    return if (firstExpireDate == null || secondExpireDate == null) {
                        0
                    } else if (firstExpireDate.before(secondExpireDate)) {
                        -1
                    } else if (secondExpireDate.before(firstExpireDate)) {
                        1
                    } else {
                        0
                    }
                } else {
                    return if (first.data.priority < second.data.priority) -1 else 1
                }
            }
        } catch (e: Exception) {
            return 0
        }
    }
}
