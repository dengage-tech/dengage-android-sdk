package com.dengage.sdk.domain.base

class UseCaseBuilder<Response, Params> {
    var params: Params? = null
    var onStart: (() -> Unit)? = null
    var onResponse: ((Response) -> Unit)? = null
    var onError: ((Throwable) -> Unit)? = null
    var onComplete: (() -> Unit)? = null
}