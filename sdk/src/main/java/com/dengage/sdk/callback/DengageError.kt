package com.dengage.sdk.callback

import java.io.Serializable

data class DengageError(
    var errorMessage: String?
) : Serializable
