package com.dengage.sdk.domain.base

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.flow.flow
import kotlinx.coroutines.flow.flowOn
import kotlinx.coroutines.flow.onCompletion
import kotlinx.coroutines.flow.onEach
import kotlinx.coroutines.flow.onStart
import kotlinx.coroutines.launch

abstract class CoroutineUseCase<T, Params> : BaseUseCase<T, Params> {

    private var scope: CoroutineScope = CoroutineScope(Dispatchers.Main)
    private val dispatcher = Dispatchers.IO
    private var job: Job? = null

    override fun execute(
        useCaseRunner: UseCaseRunner,
        callback: Callback<T>?,
        params: Params?
    ) {
        useCaseRunner.subscribeToCancel {
            cancel()
        }

        job = scope.launch {
            flow<Result<T>> {
                runCatching {
                    buildUseCase(params)
                }.onSuccess {
                    emit(Result.Success(it))
                }.onFailure {
                    emit(Result.Error(it))
                }
            }
                .flowOn(dispatcher)
                .onStart {
                    callback?.onStart()
                }
                .onEach {
                    when (it) {
                        is Result.Success -> callback?.onResponse(it.data)
                        is Result.Error -> callback?.onError(it.error)
                    }
                }
                .onCompletion {
                    callback?.onComplete()
                }.collect()
        }
    }

    override fun cancel() {
        job?.cancel()
    }

    abstract suspend fun buildUseCase(params: Params?): T
}

private sealed class Result<out T> {
    data class Success<out T>(val data: T) : Result<T>()
    data class Error(val error: Throwable) : Result<Nothing>()
}
