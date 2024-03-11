package com.dengage.sdk.ui.inappmessage

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Context
import android.content.Intent
import android.graphics.Color
import android.net.Uri
import android.util.AttributeSet
import android.view.ViewGroup
import android.webkit.JavascriptInterface
import android.webkit.WebView
import android.widget.RelativeLayout
import android.widget.Toast
import com.dengage.sdk.Dengage
import com.dengage.sdk.callback.ReviewDialogCallback
import com.dengage.sdk.domain.inappmessage.model.ContentParams
import com.dengage.sdk.domain.inappmessage.model.ContentPosition
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.push.areNotificationsEnabled
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.extension.launchActivity
import com.dengage.sdk.util.extension.launchSettingsActivity

class InAppInlineElement : WebView {
    private lateinit var inAppMessage: InAppMessage
    private var contextWebView: Context? = null
    private var isAndroidUrlNPresent: Boolean? = false
    private var isRatingDialog: Boolean? = false
    private var isClicked: Boolean = false
    var activityContext: Activity? = null

    constructor(context: Context?) : super(context!!) {
        contextWebView = context
    }

    constructor(context: Context?, attrs: AttributeSet?) : super(
        context!!, attrs) {
        contextWebView = context
    }

    companion object {
        /**
        Set Inapp inlinee callback for handling Inapp inlinee actions
         */
        var inAppMessageCallback: InAppMessageCallback? = null


        const val EXTRA_IN_APP_MESSAGE = "EXTRA_IN_APP_MESSAGE"
        const val RESULT_CODE = "RESULT_CODE"


    }

    interface InAppMessageCallback {
        /**
        Clicked Inapp inlinee
         */
        fun inAppMessageClicked(inAppMessage: InAppMessage, buttonId: String?)

        /**
        Dismissed Inapp inlinee
         */
        fun inAppMessageDismissed(inAppMessage: InAppMessage)

        /**
        Send tags method for using from webview javascript interface
         */
        fun sendTags(tags: String?)
    }

    fun populateInLineInApp(inAppMessageParam: InAppMessage, activityParam: Activity) {
        inAppMessage = inAppMessageParam
        activityContext = activityParam
        val contentParams = inAppMessage.data.content.params
        setHtmlContent(contentParams)

    }


    @SuppressLint("SetJavaScriptEnabled")
    private fun setHtmlContent(contentParams: ContentParams) {
        if (contentParams.position == ContentPosition.FULL.position) {
            val params = RelativeLayout.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT
            )
            this.layoutParams = params
        }

        // set radius of card view
        //cardInAppMessage.radius = InAppMessageUtils.pxToDp(contentParams.radius, contextWebView!! )

        // set max width for container
        /* contentParams.maxWidth?.let {
             val params = vHtmlWidthContainer.layoutParams as ConstraintLayout.LayoutParams
             params.matchConstraintMaxWidth = InAppMessageUtils.pxToDp(it, contextWebView!! ).roundToInt()
             vHtmlWidthContainer.layoutParams = params
         }*/

        // vHtmlContent.visibility = View.VISIBLE


        this.apply {

            contentParams.html?.let {
                loadDataWithBaseURL(
                    null,
                    it, "text/html", "UTF-8", null
                )
            }
            settings.loadWithOverviewMode = true
            settings.useWideViewPort = true
            settings.displayZoomControls = false
            settings.builtInZoomControls = true
            settings.setSupportZoom(true)
            setBackgroundColor(Color.TRANSPARENT)
            settings.domStorageEnabled = true
            settings.javaScriptEnabled = true
            settings.javaScriptCanOpenWindowsAutomatically = true
            this.addJavascriptInterface(JavaScriptInterface(), "Dn")
        }
    }

    private inner class JavaScriptInterface {
        @JavascriptInterface
        fun dismiss() {
            DengageLogger.verbose("Inapp inlinee: dismiss event")
        }

        @JavascriptInterface
        fun androidUrl(targetUrl: String) {
            DengageLogger.verbose("Inapp inlinee: android target url event $targetUrl")
            if (targetUrl == "Dn.promptPushPermission()") {
                if (!context.areNotificationsEnabled()) {
                    Toast.makeText(context, "You need to enable push permission", Toast.LENGTH_LONG).show()
                    context.launchSettingsActivity ()
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
        fun sendClick(buttonId: String?) {
            isClicked = true
            DengageLogger.verbose ("Inapp inlinee: clicked button $buttonId")
            inAppMessageCallback?.inAppMessageClicked (inAppMessage, buttonId)
        }

        @JavascriptInterface
        fun sendClick() {
            isClicked = true
            DengageLogger.verbose ("Inapp inlinee: clicked body/button with no Id")
            inAppMessageCallback?.inAppMessageClicked (inAppMessage, null)
        }

        @JavascriptInterface
        fun close() {
        }

        @JavascriptInterface
        fun setTags() {
            DengageLogger.verbose("Inapp inlinee: set tags event")
        }

        @JavascriptInterface
        fun iosUrl(targetUrl: String) {
            DengageLogger.verbose("Inapp inlinee: ios target url event $targetUrl")
        }

        @JavascriptInterface
        fun promptPushPermission() {
            DengageLogger.verbose("Inapp inlinee: prompt push permission event")
            if (!context.areNotificationsEnabled()) {
                Toast.makeText(context, "You need to enable push permission", Toast.LENGTH_LONG).show()
                context.launchSettingsActivity ()
            }
        }
    }

    fun showRating() {
        activityContext?.let {
            Dengage.showRatingDialog(activity = it,
                reviewDialogCallback = object : ReviewDialogCallback {
                    override fun onCompletion() {
                    }

                    override fun onError() {

                    }

                })
        }

    }
}