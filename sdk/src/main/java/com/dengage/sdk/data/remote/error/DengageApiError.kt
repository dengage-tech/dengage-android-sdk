package com.dengage.sdk.data.remote.error

class DengageApiError(
    val throwable: Throwable,
    val errorType: ErrorType,
    val statusCode: Int? = null,
    val errorCode: Any? = null,
    val errorMessage: String?,
) : RuntimeException() {

    override fun toString(): String {
        return "$errorType-$throwable"
    }

    companion object {
        fun connectionError(throwable: Throwable): DengageApiError {
            return DengageApiError(
                throwable = throwable,
                errorType = ErrorType.CONNECTION_ERROR,
                errorMessage = throwable.message
            )
        }

        fun timeOutError(throwable: Throwable): DengageApiError {
            return DengageApiError(
                throwable = throwable,
                errorType = ErrorType.TIME_OUT_ERROR,
                errorMessage = throwable.message
            )
        }

        fun serverError(throwable: Throwable, statusCode: Int?): DengageApiError {
            return DengageApiError(
                throwable = throwable,
                errorType = ErrorType.SERVER_ERROR,
                errorMessage = throwable.message,
                statusCode = statusCode,
            )
        }

        fun apiError(
            throwable: Throwable,
            statusCode: Int?,
            errorCode: Any?,
            errorMessage: String?
        ): DengageApiError {
            return DengageApiError(
                throwable = throwable,
                errorType = ErrorType.API_ERROR,
                statusCode = statusCode,
                errorCode = errorCode,
                errorMessage = errorMessage,
            )
        }

        fun unknownError(throwable: Throwable): DengageApiError {
            return DengageApiError(
                throwable = throwable,
                errorType = ErrorType.UNKNOWN_ERROR,
                errorMessage = throwable.message
            )
        }
    }
}

enum class ErrorType {
    CONNECTION_ERROR,
    TIME_OUT_ERROR,
    SERVER_ERROR,
    API_ERROR,
    UNKNOWN_ERROR
}
