package com.dengage.sdk.ui.inappmessage

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Intent
import android.content.res.Resources
import android.graphics.Color
import android.net.Uri
import android.os.Bundle
import android.view.View
import android.view.ViewGroup.LayoutParams.MATCH_PARENT
import android.view.ViewGroup.LayoutParams.WRAP_CONTENT
import android.webkit.JavascriptInterface
import android.webkit.WebView
import android.widget.RelativeLayout
import android.widget.Toast
import androidx.cardview.widget.CardView
import androidx.constraintlayout.widget.ConstraintLayout
import com.dengage.sdk.R
import com.dengage.sdk.data.cache.Prefs
import com.dengage.sdk.domain.inappmessage.model.ContentParams
import com.dengage.sdk.domain.inappmessage.model.ContentPosition
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.manager.inappmessage.util.InAppMessageUtils
import com.dengage.sdk.push.areNotificationsEnabled
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.extension.launchActivity
import com.dengage.sdk.util.extension.launchSettingsActivity
import kotlin.math.roundToInt


class InAppMessageActivity : Activity(), View.OnClickListener {

    private lateinit var inAppMessage: InAppMessage
    private var isAndroidUrlNPresent: Boolean? =false;

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_in_app_message)

        inAppMessage = intent.getSerializableExtra(EXTRA_IN_APP_MESSAGE) as InAppMessage
        val contentParams = inAppMessage.data.content.params

        setContentPosition(contentParams)
        setHtmlContent(contentParams)

        findViewById<View>(R.id.vInAppMessageContainer).setOnClickListener(this)
        findViewById<View>(R.id.cardInAppMessage).setOnClickListener(this)
    }

    private fun setContentPosition(
        contentParams: ContentParams
    ) {
        val cardInAppMessage = findViewById<CardView>(R.id.cardInAppMessage)
        val params = RelativeLayout.LayoutParams(
            WRAP_CONTENT,
            WRAP_CONTENT
        )
        val screenWidth = Resources.getSystem().displayMetrics.widthPixels
        val screenHeight = Resources.getSystem().displayMetrics.heightPixels
        params.setMargins(
            InAppMessageUtils.getPixelsByPercentage(screenWidth, contentParams.marginLeft),
            InAppMessageUtils.getPixelsByPercentage(screenHeight, contentParams.marginTop),
            InAppMessageUtils.getPixelsByPercentage(screenWidth, contentParams.marginRight),
            InAppMessageUtils.getPixelsByPercentage(screenHeight, contentParams.marginBottom)
        )
        params.addRule(RelativeLayout.CENTER_HORIZONTAL)
        when (contentParams.position) {
            ContentPosition.BOTTOM.position -> {
                params.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM)
            }
            ContentPosition.MIDDLE.position -> {
                params.addRule(RelativeLayout.CENTER_VERTICAL)
            }
            ContentPosition.TOP.position -> {
                params.addRule(RelativeLayout.ALIGN_PARENT_TOP)
            }
        }
        cardInAppMessage.layoutParams = params
    }

    @SuppressLint("SetJavaScriptEnabled")
    private fun setHtmlContent(contentParams: ContentParams) {
        val vHtmlContent = findViewById<View>(R.id.vHtmlContent)
        val webView = findViewById<WebView>(R.id.webView)
        val vHtmlWidthContainer = findViewById<RelativeLayout>(R.id.vHtmlWidthContainer)
        val cardInAppMessage = findViewById<CardView>(R.id.cardInAppMessage)

        // set height for content type full
        if (contentParams.position == ContentPosition.FULL.position) {
            val params = RelativeLayout.LayoutParams(
                MATCH_PARENT,
                MATCH_PARENT
            )
            webView.layoutParams = params
        }

        // set radius of card view
        cardInAppMessage.radius = InAppMessageUtils.pxToDp(contentParams.radius, this)

        // set max width for container
        contentParams.maxWidth?.let {
            val params = vHtmlWidthContainer.layoutParams as ConstraintLayout.LayoutParams
            params.matchConstraintMaxWidth = InAppMessageUtils.pxToDp(it, this).roundToInt()
            vHtmlWidthContainer.layoutParams = params
        }

        vHtmlContent.visibility = View.VISIBLE

        isAndroidUrlNPresent = contentParams.html?.contains("Dn.androidUrlN")

        webView.apply {

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
            addJavascriptInterface(JavaScriptInterface(), "Dn")
        }
    }

    override fun onClick(v: View?) {
        when (v?.id) {
            R.id.vInAppMessageContainer -> {
                if (inAppMessage.data.content.params.dismissOnTouchOutside != false) {
                    finish()
                }
            }
            R.id.cardInAppMessage -> {
                // ignore
            }
        }
    }

    private fun inAppMessageDismissed() {
        inAppMessageCallback?.inAppMessageDismissed(inAppMessage)
    }

    override fun onDestroy() {
        inAppMessageDismissed()
        inAppMessageCallback = null
        super.onDestroy()
    }

    override fun onPause() {
        super.onPause()
        if (!inAppMessage.data.content.params.shouldAnimate) {
            overridePendingTransition(0, 0)
        }
    }

    interface InAppMessageCallback {
        /**
        Clicked in app message
         */
        fun inAppMessageClicked(inAppMessage: InAppMessage, buttonId: String?)

        /**
        Dismissed in app message
         */
        fun inAppMessageDismissed(inAppMessage: InAppMessage)

        /**
        Send tags method for using from webview javascript interface
         */
        fun sendTags(tags: String?)
    }

    companion object {
        /**
        Set in app message callback for handling in app message actions
         */
        var inAppMessageCallback: InAppMessageCallback? = null

        const val EXTRA_IN_APP_MESSAGE = "EXTRA_IN_APP_MESSAGE"
        const val RESULT_CODE = "RESULT_CODE"

        fun newIntent(activity: Activity, inAppMessage: InAppMessage, resultCode: Int): Intent {
            val intent = Intent(activity, InAppMessageActivity::class.java).apply {
                putExtra(EXTRA_IN_APP_MESSAGE, inAppMessage)
                putExtra(RESULT_CODE, resultCode)
            }
            return intent
        }
    }


    private inner class JavaScriptInterface {
        @JavascriptInterface
        fun dismiss() {
            DengageLogger.verbose("In app message: dismiss event")
            this@InAppMessageActivity.finish()
        }

        @JavascriptInterface
        fun androidUrl(targetUrl: String) {
            if(isAndroidUrlNPresent == false) {
                DengageLogger.verbose("In app message: android target url event $targetUrl")

                if (targetUrl == "Dn.promptPushPermission()") {
                    if (!this@InAppMessageActivity.areNotificationsEnabled()) {
                        Toast.makeText(
                            this@InAppMessageActivity,
                            "You need to enable push permission",
                            Toast.LENGTH_LONG
                        ).show()
                        this@InAppMessageActivity.launchSettingsActivity()
                    }
                } else {
                    try {
                        this@InAppMessageActivity.launchActivity(null, targetUrl)
                    } catch (e: Exception) {
                        e.printStackTrace()
                    }
                }
            }


        }

        @JavascriptInterface
        fun androidUrlN(targetUrl: String,inAppBrowser : Boolean , retrieveOnSameLink : Boolean) {
            DengageLogger.verbose("In app message: android target url n event $targetUrl")

            if (targetUrl == "Dn.promptPushPermission()") {
                if (!this@InAppMessageActivity.areNotificationsEnabled()) {
                    Toast.makeText(
                        this@InAppMessageActivity,
                        "You need to enable push permission",
                        Toast.LENGTH_LONG
                    ).show()
                    this@InAppMessageActivity.launchSettingsActivity()
                }
            } else if (DengageUtils.isDeeplink(targetUrl)) {

                try {
                    if (retrieveOnSameLink) {
                        val intent = Intent(Intent.ACTION_VIEW, Uri.parse(targetUrl))
                        intent.putExtra("targetUrl", targetUrl)
                        intent.extras?.let { setResult(it.getInt(RESULT_CODE), intent) }
                    } else {
                        this@InAppMessageActivity.launchActivity(null, targetUrl)
                    }
                } catch (e: Exception) {
                    DengageLogger.error(e.message)
                }
            } else if (retrieveOnSameLink && !inAppBrowser) {
                val intent = Intent(Intent.ACTION_VIEW, Uri.parse(targetUrl))
                intent.putExtra("targetUrl", targetUrl)
                intent.extras?.let { setResult(it.getInt(RESULT_CODE), intent) }
            } else if (inAppBrowser) {
                val intent = InAppBrowserActivity.Builder.getBuilder()
                    .withUrl(targetUrl)
                    .build(this@InAppMessageActivity)

                startActivity(intent)
            } else {
                this@InAppMessageActivity.launchActivity(null, targetUrl)
            }

        }

        @JavascriptInterface
        fun sendClick(buttonId: String?) {
            DengageLogger.verbose("In app message: clicked button $buttonId")
            inAppMessageCallback?.inAppMessageClicked(inAppMessage, buttonId)
        }

        @JavascriptInterface
        fun sendClick() {
            DengageLogger.verbose("In app message: clicked body/button with no Id")
            inAppMessageCallback?.inAppMessageClicked(inAppMessage, null)
        }

        @JavascriptInterface
        fun close() {
            if(isAndroidUrlNPresent == false) {
                DengageLogger.verbose("In app message: close event")
                this@InAppMessageActivity.finish()
            }
        }

        @JavascriptInterface
        fun closeN() {
            DengageLogger.verbose("In app message: close event n")
            this@InAppMessageActivity.finish()
        }

        @JavascriptInterface
        fun setTags() {
            DengageLogger.verbose("In app message: set tags event")
        }

        @JavascriptInterface
        fun iosUrl(targetUrl: String) {
            DengageLogger.verbose("In app message: ios target url event $targetUrl")
        }

        @JavascriptInterface
        fun iosUrlN(targetUrl: String,inAppBrowser : Boolean , retrieveOnSameLink : Boolean) {
            DengageLogger.verbose("In app message: ios target url n event $targetUrl")
        }

        @JavascriptInterface
        fun promptPushPermission() {
            DengageLogger.verbose("In app message: prompt push permission event")
            if (!this@InAppMessageActivity.areNotificationsEnabled()) {
                Toast.makeText(
                    this@InAppMessageActivity,
                    "You need to enable push permission",
                    Toast.LENGTH_LONG
                ).show()
                this@InAppMessageActivity.launchSettingsActivity()
            }
        }
    }

}