package com.dengage.sdk.domain.event.model

/**
 * Push notification events reported to the push api. Open and dismiss share the same request
 * body and the same regular/transactional routing; only the endpoint differs.
 */
enum class PushEventType(val path: String, val transactionalPath: String) {
    OPEN(PushEventPaths.OPEN, PushEventPaths.TRANSACTIONAL_OPEN),
    DISMISS(PushEventPaths.DISMISS, PushEventPaths.TRANSACTIONAL_DISMISS);

    fun path(isTransactional: Boolean): String = if (isTransactional) transactionalPath else path
}

object PushEventPaths {
    const val OPEN = "/api/mobile/open"
    const val TRANSACTIONAL_OPEN = "/api/transactional/mobile/open"
    const val DISMISS = "/api/mobile/dismiss"
    const val TRANSACTIONAL_DISMISS = "/api/transactional/mobile/dismiss"
}
