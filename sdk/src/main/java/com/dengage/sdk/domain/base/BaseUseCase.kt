package com.dengage.sdk.domain.base

interface BaseUseCase<T, Params> {

    operator fun invoke(
        useCaseRunner: UseCaseRunner,
        useCaseBuilderBlock: UseCaseBuilder<T, Params>.() -> Unit
    ) = execute(useCaseRunner, useCaseBuilderBlock)

    fun execute(
        useCaseRunner: UseCaseRunner,
        useCaseBuilderBlock: UseCaseBuilder<T, Params>.() -> Unit
    ) {
        val useCaseBuilder = UseCaseBuilder<T, Params>().apply(useCaseBuilderBlock)
        execute(
            useCaseRunner = useCaseRunner,
            callback = useCaseRunner.callback(
                onStart = useCaseBuilder.onStart,
                onResponse = useCaseBuilder.onResponse,
                onError = useCaseBuilder.onError,
                onComplete = useCaseBuilder.onComplete
            ),
            params = useCaseBuilder.params
        )
    }

    fun execute(
        useCaseRunner: UseCaseRunner,
        callback: Callback<T>?,
        params: Params? = null
    )

    fun cancel()
}