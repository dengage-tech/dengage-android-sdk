package com.dengage.sdk.domain.base

interface UseCaseRunner {

    fun cancelUseCases()

    fun <T> callback(
        onStart: (() -> Unit)?,
        onResponse: ((T) -> Unit)?,
        onError: ((Throwable) -> Unit)?,
        onComplete: (() -> Unit)?,
    ): Callback<T> = Callback(
        onStart = onStart,
        onResponse = onResponse,
        onError = onError,
        onComplete = onComplete
    )

    fun subscribeToCancel(onCancel: (Boolean) -> Unit)
}

class UseCaseRunnerDelegate : UseCaseRunner {
    private val onCancelCallbacks = arrayListOf<OnCancel>()

    override fun cancelUseCases() {
        val iterator = onCancelCallbacks.iterator()
        while (iterator.hasNext()) {
            iterator.next().invoke(true)
            iterator.remove()
        }
    }

    override fun subscribeToCancel(onCancel: OnCancel) {
        onCancelCallbacks.add(onCancel)
    }
}

typealias OnCancel = (Boolean) -> Unit
