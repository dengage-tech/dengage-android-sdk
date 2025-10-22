package com.dengage.android.kotlin.sample.ui.model

data class EventParameter(
    var key: String,
    var value: String,
    var isReadOnly: Boolean = false,
    var inputType: InputType = InputType.TEXT,
    var options: List<String> = emptyList()
) {
    enum class InputType {
        TEXT,
        DROPDOWN
    }
}