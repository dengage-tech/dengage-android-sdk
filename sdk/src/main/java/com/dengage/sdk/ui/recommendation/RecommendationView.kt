package com.dengage.sdk.ui.recommendation

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Context
import android.util.AttributeSet
import android.view.ViewGroup
import android.webkit.JavascriptInterface
import android.webkit.WebView
import android.webkit.WebViewClient
import android.widget.Toast
import com.dengage.sdk.Dengage
import com.dengage.sdk.ui.inappmessage.bridge.core.DengageBridge
import com.dengage.sdk.ui.inappmessage.bridge.handler.BridgeHandlerRegistry
import com.dengage.sdk.ui.inappmessage.bridge.handlers.DeviceInfoHandler
import com.dengage.sdk.ui.inappmessage.bridge.handlers.RecommendationHandler
import com.dengage.sdk.ui.inappmessage.bridge.handlers.HttpRequestHandler
import com.dengage.sdk.ui.inappmessage.bridge.handlers.LegacyDnHandler
import com.dengage.sdk.ui.inappmessage.bridge.handlers.StorageHandler
import com.dengage.sdk.ui.inappmessage.bridge.js.BridgeJavaScript
import com.dengage.sdk.callback.ReviewDialogCallback
import com.dengage.sdk.domain.inappmessage.model.ContentParams
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.push.areNotificationsEnabled
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.extension.launchActivity
import com.dengage.sdk.util.extension.launchNotificationSettingsActivity

open class RecommendationView : WebView {
    private lateinit var inAppMessage: InAppMessage
    private var contextWebView: Context? = null
    private var isClicked: Boolean = false
    var activityContext: Activity? = null
    private var dengageBridge: DengageBridge? = null

    constructor(context: Context?) : super(context!!) {
        contextWebView = context
    }

    constructor(context: Context?, attrs: AttributeSet?) : super(context!!, attrs) {
        contextWebView = context
    }

    companion object {
        var inAppMessageCallback: InAppMessageCallback? = null
    }

    interface InAppMessageCallback {
        fun inAppMessageClicked(inAppMessage: InAppMessage, buttonId: String?, buttonType: String?)
        fun inAppMessageDismissed(inAppMessage: InAppMessage)
        fun sendTags(tags: List<TagItem>?)
    }

    internal fun populateRecommendation(inAppMessageParam: InAppMessage, activityParam: Activity) {
        inAppMessage = inAppMessageParam
        activityContext = activityParam
        val contentParams = inAppMessage.data.content.params
        setHtmlContent(contentParams)
    }

    @SuppressLint("SetJavaScriptEnabled")
    private fun setHtmlContent(contentParams: ContentParams) {
        this.apply {
            settings.loadWithOverviewMode = true
            settings.useWideViewPort = true
            settings.displayZoomControls = false
            settings.builtInZoomControls = true
            settings.setSupportZoom(true)
            //setBackgroundColor(Color.TRANSPARENT)
            settings.domStorageEnabled = true
            settings.javaScriptEnabled = true
            settings.javaScriptCanOpenWindowsAutomatically = true

            contextWebView?.let { ctx ->
                val legacyHandler = LegacyDnHandler(
                    context = ctx,
                    activity = activityContext,
                    inAppMessage = inAppMessage,
                    inAppMessageCallback = null,
                    isAndroidUrlNPresent = false,
                    isRatingDialog = false,
                    onClicked = { isClicked = true }
                )

                val registry = BridgeHandlerRegistry().apply {
                    register(legacyHandler)
                    register(HttpRequestHandler(inAppMessage))
                    register(DeviceInfoHandler())
                    register(StorageHandler())
                    register(RecommendationHandler())
                }

                dengageBridge = DengageBridge.attach(this@RecommendationView, registry)
            }

            this.addJavascriptInterface(JavaScriptInterface(), "Dn")
            this.addJavascriptInterface(HeightInterface(), "DnRecommendationView")

            webViewClient = object : WebViewClient() {
                override fun onPageFinished(view: WebView?, url: String?) {
                    super.onPageFinished(view, url)
                    BridgeJavaScript.inject(this@RecommendationView)
                    scheduleHeightAdjustment()
                }
            }

            contentParams.html?.let { html ->
                val injectedHtml = BridgeJavaScript.injectIntoHtml(html)
                loadDataWithBaseURL(null, injectedHtml, "text/html", "UTF-8", null)
            }
        }
    }

    private fun scheduleHeightAdjustment() {
        val observerJs = """
            (function() {
                var lastHeight = 0;
                function reportHeight() {
                    var h = document.body.scrollHeight;
                    if (h !== lastHeight && h > 0) {
                        lastHeight = h;
                        window.DnRecommendationView.onContentHeightChanged(h);
                    }
                }
                new MutationObserver(function() {
                    reportHeight();
                }).observe(document.body, { childList: true, subtree: true, attributes: true });
                reportHeight();
            })();
        """.trimIndent()
        postDelayed({ evaluateJavascript(observerJs, null) }, 300)
    }

    private inner class HeightInterface {
        @JavascriptInterface
        fun onContentHeightChanged(height: Int) {
            adjustHeight(height)
        }
    }

    private fun adjustHeight(contentHeightPx: Int) {
        try {
            if (contentHeightPx <= 0) return
            val density = context.resources.displayMetrics.density
            val heightInPx = (contentHeightPx * density).toInt()
            post {
                layoutParams = layoutParams.apply {
                    height = heightInPx
                }
                requestLayout()
            }
        } catch (e: Exception) {
            DengageLogger.error("RecommendationView adjustHeight error: ${e.message}")
        }
    }

    private inner class JavaScriptInterface {
        @JavascriptInterface
        fun dismiss() {
            DengageLogger.verbose("RecommendationView: dismiss event")
        }

        @JavascriptInterface
        fun androidUrl(targetUrl: String) {
            DengageLogger.verbose("RecommendationView: android target url event $targetUrl")
            if (targetUrl == "Dn.promptPushPermission()") {
                if (!context.areNotificationsEnabled()) {
                    Toast.makeText(context, "You need to enable push permission", Toast.LENGTH_LONG).show()
                    context.launchNotificationSettingsActivity()
                }
            } else {
                try {
                    context.launchActivity(null, targetUrl)
                } catch (e: Exception) {
                    e.printStackTrace()
                }
            }
        }

        @JavascriptInterface
        fun sendClick(buttonId: String?, buttonType: String?) {
            isClicked = true
            DengageLogger.verbose("RecommendationView: clicked button $buttonId $buttonType")
            inAppMessageCallback?.inAppMessageClicked(inAppMessage, buttonId, buttonType)
        }

        @JavascriptInterface
        fun sendClick(buttonId: String?) {
            isClicked = true
            DengageLogger.verbose("RecommendationView: clicked button $buttonId")
            inAppMessageCallback?.inAppMessageClicked(inAppMessage, buttonId, null)
        }

        @JavascriptInterface
        fun sendClick() {
            isClicked = true
            DengageLogger.verbose("RecommendationView: clicked body/button with no Id")
            inAppMessageCallback?.inAppMessageClicked(inAppMessage, null, null)
        }

        @JavascriptInterface
        fun close() {
        }

        @JavascriptInterface
        fun setTags() {
            DengageLogger.verbose("RecommendationView: set tags event")
        }

        @JavascriptInterface
        fun iosUrl(targetUrl: String) {
            DengageLogger.verbose("RecommendationView: ios target url event $targetUrl")
        }

        @JavascriptInterface
        fun promptPushPermission() {
            DengageLogger.verbose("RecommendationView: prompt push permission event")
            if (!context.areNotificationsEnabled()) {
                Toast.makeText(context, "You need to enable push permission", Toast.LENGTH_LONG).show()
                context.launchNotificationSettingsActivity()
            }
        }
    }

    fun showRating() {
        activityContext?.let {
            Dengage.showRatingDialog(activity = it,
                reviewDialogCallback = object : ReviewDialogCallback {
                    override fun onCompletion() {}
                    override fun onError() {}
                })
        }
    }
}
