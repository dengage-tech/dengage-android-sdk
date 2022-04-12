package com.dengage.android.kotlin.sample.ui.model

import com.dengage.sdk.domain.rfm.model.RFMGender
import com.dengage.sdk.domain.rfm.model.RFMItem

class AppRFMItem(
    id: String,
    categoryId: String,
    personalized: Boolean,
    gender: RFMGender,
    sequence: Int,
    var parameter1: String,
    var parameter2: String,
) : RFMItem(
    id = id,
    categoryId = categoryId,
    personalized = personalized,
    gender = gender,
    sequence = sequence,
)