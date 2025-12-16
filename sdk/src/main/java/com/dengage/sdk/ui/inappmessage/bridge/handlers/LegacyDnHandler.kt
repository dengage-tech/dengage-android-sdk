package com.dengage.sdk.ui.inappmessage.bridge.handlers

import android.app.Activity
import android.content.Context
import android.content.Intent
import android.net.Uri
import android.widget.Toast
import com.dengage.sdk.Dengage
import com.dengage.sdk.ui.inappmessage.bridge.core.BridgeMessage
import com.dengage.sdk.ui.inappmessage.bridge.handler.FireAndForgetHandler
import com.dengage.sdk.callback.ReviewDialogCallback
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.push.areNotificationsEnabled
import com.dengage.sdk.ui.inappmessage.InAppBrowserActivity
import com.dengage.sdk.ui.inappmessage.InAppMessageActivity
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.GsonHolder
import com.dengage.sdk.util.extension.launchActivity
import com.dengage.sdk.util.extension.launchNotificationSettingsActivity

/**
 * Handler for legacy Dn.* JavaScript interface methods
 * Provides backwards compatibility with existing HTML content
 */
class LegacyDnHandler(
    private val context: Context,
    private val activity: Activity? = null,
    private val inAppMessage: InAppMessage? = null,
    private val inAppMessageCallback: InAppMessageActivity.InAppMessageCallback? = null,
    private val isAndroidUrlNPresent: Boolean? = false,
    private val isRatingDialog: Boolean? = false,
    private val onClicked: (() -> Unit)? = null,
    private val onFinish: (() -> Unit)? = null
) : FireAndForgetHandler {

    override fun supportedActions(): List<String> = listOf(
        "legacy_dismiss",
        "legacy_androidUrl",
        "legacy_androidUrlN",
        "legacy_sendClick",
        "legacy_close",
        "legacy_closeN",
        "legacy_setTags",
        "legacy_iosUrl",
        "legacy_iosUrlN",
        "legacy_promptPushPermission",
        "legacy_showRating"
    )

    // Payload data classes
    data class AndroidUrlPayload(val targetUrl: String)
    data class AndroidUrlNPayload(
        val targetUrl: String,
        val inAppBrowser: Boolean,
        val retrieveOnSameLink: Boolean
    )
    data class SendClickPayload(val buttonId: String?, val buttonType: String?)

    override fun handle(message: BridgeMessage) {
        try {
            when (message.action) {
                "legacy_dismiss" -> handleDismiss()
                "legacy_androidUrl" -> {
                    val payload = GsonHolder.fromJson<AndroidUrlPayload>(message.payload)
                    payload?.let { handleAndroidUrl(it.targetUrl) }
                }
                "legacy_androidUrlN" -> {
                    val payload = GsonHolder.fromJson<AndroidUrlNPayload>(message.payload)
                    payload?.let { handleAndroidUrlN(it.targetUrl, it.inAppBrowser, it.retrieveOnSameLink) }
                }
                "legacy_sendClick" -> {
                    val payload = GsonHolder.fromJson<SendClickPayload>(message.payload)
                    handleSendClick(payload?.buttonId, payload?.buttonType)
                }
                "legacy_close" -> handleClose()
                "legacy_closeN" -> handleCloseN()
                "legacy_setTags" -> handleSetTags()
                "legacy_iosUrl" -> {
                    val payload = GsonHolder.fromJson<AndroidUrlPayload>(message.payload)
                    payload?.let { DengageLogger.verbose("Legacy iosUrl: ${it.targetUrl}") }
                }
                "legacy_iosUrlN" -> {
                    val payload = GsonHolder.fromJson<AndroidUrlNPayload>(message.payload)
                    payload?.let { DengageLogger.verbose("Legacy iosUrlN: ${it.targetUrl}") }
                }
                "legacy_promptPushPermission" -> handlePromptPushPermission()
                "legacy_showRating" -> handleShowRating()
            }
        } catch (e: Exception) {
            DengageLogger.error("LegacyDnHandler error: ${e.message}")
        }
    }

    private fun handleDismiss() {
        DengageLogger.verbose("In app message: dismiss event")
        onFinish?.invoke()
    }

    private fun handleAndroidUrl(targetUrl: String) {
        if (isAndroidUrlNPresent == false) {
            DengageLogger.verbose("In app message: android target url event $targetUrl")
            if (targetUrl == "Dn.promptPushPermission()") {
                handlePromptPushPermission()
            } else {
                try {
                    context.launchActivity(null, targetUrl)
                } catch (e: Exception) {
                    e.printStackTrace()
                }
            }
        }
    }

    private fun handleAndroidUrlN(targetUrl: String, inAppBrowser: Boolean, retrieveOnSameLink: Boolean) {
        DengageLogger.verbose("In app message: android target url n event $targetUrl")

        when {
            targetUrl.equals("DN.SHOWRATING()", ignoreCase = true) -> handleShowRating()
            targetUrl == "Dn.promptPushPermission()" -> handlePromptPushPermission()
            DengageUtils.isDeeplink(targetUrl) -> handleDeeplink(targetUrl, retrieveOnSameLink)
            retrieveOnSameLink && !inAppBrowser -> handleRetrieveOnSameLink(targetUrl)
            inAppBrowser -> handleInAppBrowser(targetUrl)
            else -> context.launchActivity(null, targetUrl)
        }
    }

    private fun handleDeeplink(targetUrl: String, retrieveOnSameLink: Boolean) {
        try {
            if (retrieveOnSameLink) {
                val intent = Intent(Intent.ACTION_VIEW, Uri.parse(targetUrl))
                intent.putExtra("targetUrl", targetUrl)
                DengageUtils.sendBroadCast(intent.apply {
                    this.action = Constants.DEEPLINK_RETRIEVE_EVENT
                }, context)
            } else {
                context.launchActivity(null, targetUrl)
            }
        } catch (e: Exception) {
            DengageLogger.error(e.message)
        }
    }

    private fun handleRetrieveOnSameLink(targetUrl: String) {
        val intent = Intent(Intent.ACTION_VIEW, Uri.parse(targetUrl))
        intent.putExtra("targetUrl", targetUrl)
        DengageUtils.sendBroadCast(intent.apply {
            this.action = Constants.DEEPLINK_RETRIEVE_EVENT
        }, context)
    }

    private fun handleInAppBrowser(targetUrl: String) {
        activity?.let {
            val intent = InAppBrowserActivity.Builder.getBuilder()
                .withUrl(targetUrl)
                .build(it)
            it.startActivity(intent)
        }
    }

    private fun handleSendClick(buttonId: String?, buttonType: String?) {
        onClicked?.invoke()
        DengageLogger.verbose("In app message: clicked button $buttonId")
        inAppMessage?.let {
            inAppMessageCallback?.inAppMessageClicked(it, buttonId, buttonType)
        }
    }

    private fun handleClose() {
        if (isAndroidUrlNPresent == false && isRatingDialog == false) {
            DengageLogger.verbose("In app message: close event")
            onFinish?.invoke()
        }
    }

    private fun handleCloseN() {
        if (isRatingDialog == false) {
            DengageLogger.verbose("In app message: close event n")
            onFinish?.invoke()
        }
    }

    private fun handleSetTags() {
        DengageLogger.verbose("In app message: set tags event")
    }

    private fun handlePromptPushPermission() {
        DengageLogger.verbose("In app message: prompt push permission event")
        if (!context.areNotificationsEnabled()) {
            Toast.makeText(context, "You need to enable push permission", Toast.LENGTH_LONG).show()
            context.launchNotificationSettingsActivity()
        }
    }

    private fun handleShowRating() {
        activity?.let {
            Dengage.showRatingDialog(
                activity = it,
                reviewDialogCallback = object : ReviewDialogCallback {
                    override fun onCompletion() {}
                    override fun onError() {}
                }
            )
            onFinish?.invoke()
        }
    }
}
