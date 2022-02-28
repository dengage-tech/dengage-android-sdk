package com.dengage.sdk.util.extension

import com.dengage.sdk.data.remote.error.DengageApiError
import retrofit2.HttpException
import java.io.IOException
import java.util.concurrent.TimeoutException

fun Throwable.mapToDengageApiError(): DengageApiError {
    if (this is IOException) {
        return DengageApiError.connectionError(this)
    }

    if (this is TimeoutException) {
        return DengageApiError.timeOutError(this)
    }

    if (this is HttpException) {

        val rawResponse = this.response()
        val statusCode = rawResponse?.code()
        val errorBody = rawResponse?.errorBody()

        if (statusCode in 400..499) {
            return DengageApiError.apiError(this, statusCode, errorBody, message())
        }

        if (statusCode in 500..599) {
            return DengageApiError.serverError(this, statusCode)
        }
    }

    return DengageApiError.unknownError(this)
}
