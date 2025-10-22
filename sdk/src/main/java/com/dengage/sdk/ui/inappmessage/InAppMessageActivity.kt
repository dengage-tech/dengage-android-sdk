package com.dengage.sdk.ui.inappmessage

import android.annotation.SuppressLint
import android.app.Activity
import android.content.Intent
import android.content.res.Resources
import android.graphics.Color
import android.net.Uri
import android.os.Build
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
import kotlin.math.roundToInt
import com.dengage.sdk.Dengage
import com.dengage.sdk.R
import com.dengage.sdk.callback.ReviewDialogCallback
import com.dengage.sdk.domain.inappmessage.model.ContentParams
import com.dengage.sdk.domain.inappmessage.model.ContentPosition
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.tag.model.TagItem
import com.dengage.sdk.manager.inappmessage.util.InAppMessageUtils
import com.dengage.sdk.push.areNotificationsEnabled
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.DengageUtils
import com.dengage.sdk.util.EdgeToEdgeUtils
import com.dengage.sdk.util.extension.launchActivity
import com.dengage.sdk.util.extension.launchApplicationSettingsActivity
import com.dengage.sdk.util.extension.launchNotificationSettingsActivity

class InAppMessageActivity : Activity(), View.OnClickListener {

    private lateinit var inAppMessage: InAppMessage
    private var isAndroidUrlNPresent: Boolean? = false
    private var isRatingDialog: Boolean? = false
    private var isClicked = false

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        
        inAppMessage = intent.getSerializableExtra(EXTRA_IN_APP_MESSAGE) as? InAppMessage ?: run {
            finish()
            return
        }

        val contentParams = inAppMessage.data.content.params
        setThemeAccordingToContentParams(contentParams)
        setContentView(R.layout.activity_in_app_message)
        
        // Enable edge-to-edge display for Android 15 with proper insets handling
        //EdgeToEdgeUtils.enableEdgeToEdgeWithInsets(this)

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            contentParams.backgroundColor?.let { color ->
                if(color.isNullOrEmpty() || color.length < 3) return@let
                try {
                    window.decorView.setBackgroundColor(Color.parseColor(color.dropLast(2)))
                    window.decorView.background.alpha =
                        InAppMessageUtils.hexToPercentageOpacity(color.takeLast(2)).toInt()
                } catch (e: Exception) {
                    DengageLogger.error("InAppMessageActivity: Error parsing background color: $color")
                }
            }
        }

        setContentPosition(contentParams)
        setHtmlContent(contentParams)

        findViewById<View>(R.id.vInAppMessageContainer).setOnClickListener(this)
        findViewById<View>(R.id.cardInAppMessage).setOnClickListener(this)
    }

    private fun setThemeAccordingToContentParams(contentParams: ContentParams) {
        val isFull = contentParams.position == ContentPosition.FULL.position
        setTheme(
            if (isFull)
                R.style.Theme_AppCompat_Transparent_NoActionBar_noFloating
            else
                R.style.Theme_AppCompat_Transparent_NoActionBar
        )
    }

    private fun setContentPosition(contentParams: ContentParams) {
        val cardInAppMessage = findViewById<CardView>(R.id.cardInAppMessage)
        val params = RelativeLayout.LayoutParams(WRAP_CONTENT, WRAP_CONTENT)

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
            ContentPosition.BOTTOM.position -> params.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM)
            ContentPosition.MIDDLE.position -> params.addRule(RelativeLayout.CENTER_VERTICAL)
            ContentPosition.TOP.position    -> params.addRule(RelativeLayout.ALIGN_PARENT_TOP)
        }

        cardInAppMessage.layoutParams = params
    }

    @SuppressLint("SetJavaScriptEnabled")
    private fun setHtmlContent(contentParams: ContentParams) {
        val webView = findViewById<WebView>(R.id.webView)
        val vHtmlWidthContainer = findViewById<RelativeLayout>(R.id.vHtmlWidthContainer)
        val cardInAppMessage = findViewById<CardView>(R.id.cardInAppMessage)

        if (contentParams.position == ContentPosition.FULL.position) {
            webView.layoutParams = RelativeLayout.LayoutParams(MATCH_PARENT, MATCH_PARENT)
        }

        cardInAppMessage.radius = InAppMessageUtils.pxToDp(contentParams.radius, this)

        contentParams.maxWidth?.let {
            val params = vHtmlWidthContainer.layoutParams as ConstraintLayout.LayoutParams
            params.matchConstraintMaxWidth = InAppMessageUtils.pxToDp(it, this).roundToInt()
            vHtmlWidthContainer.layoutParams = params
        }

        findViewById<View>(R.id.vHtmlContent).visibility = View.VISIBLE
        isAndroidUrlNPresent = contentParams.html?.contains("Dn.androidUrlN")
        isRatingDialog = contentParams.html?.contains("Dn.showRating")

        with(webView.settings) {
            loadWithOverviewMode = true
            useWideViewPort = true
            displayZoomControls = false
            builtInZoomControls = true
            javaScriptEnabled = true
            domStorageEnabled = true
            javaScriptCanOpenWindowsAutomatically = true
        }

        webView.setBackgroundColor(Color.TRANSPARENT)
        webView.addJavascriptInterface(JavaScriptInterface(), "Dn")

        contentParams.html?.let { html ->
            var processedHtml = html

            val couponCode = intent.getStringExtra(EXTRA_COUPON_CODE)
            if (!couponCode.isNullOrEmpty() && Mustache.hasCouponSection(processedHtml)) {
                processedHtml = Mustache.replaceCouponSections(processedHtml, couponCode)
            }

            val dataMap = mapOf("dnInAppDeviceInfo" to Dengage.getInAppDeviceInfo())
            val renderedHtml = Mustache.render(processedHtml, dataMap)

            webView.loadDataWithBaseURL(null, renderedHtml, "text/html", "UTF-8", null)
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
                // Do nothing (ignore clicks on the card itself)
            }
        }
    }

    private fun inAppMessageDismissed() {
        inAppMessageCallback?.inAppMessageDismissed(inAppMessage)
    }

    override fun onDestroy() {
        if (!isClicked) inAppMessageDismissed()
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
        /** Clicked in app message */
        fun inAppMessageClicked(inAppMessage: InAppMessage, buttonId: String?, buttonType: String?)

        /** Dismissed in app message */
        fun inAppMessageDismissed(inAppMessage: InAppMessage)

        /** Send tags method for using from webview javascript interface */
        fun sendTags(tags: List<TagItem>?)
    }

    companion object {
        var inAppMessageCallback: InAppMessageCallback? = null

        const val EXTRA_IN_APP_MESSAGE = "EXTRA_IN_APP_MESSAGE"
        const val EXTRA_COUPON_CODE = "EXTRA_COUPON_CODE"
        const val RESULT_CODE = "RESULT_CODE"

        fun newIntent(activity: Activity, inAppMessage: InAppMessage, resultCode: Int): Intent {
            return Intent(activity, InAppMessageActivity::class.java).apply {
                putExtra(EXTRA_IN_APP_MESSAGE, inAppMessage)
                putExtra(RESULT_CODE, resultCode)
            }
        }

        fun newIntent(activity: Activity, inAppMessage: InAppMessage, resultCode: Int, couponCode: String?): Intent {
            return Intent(activity, InAppMessageActivity::class.java).apply {
                putExtra(EXTRA_IN_APP_MESSAGE, inAppMessage)
                putExtra(RESULT_CODE, resultCode)
                putExtra(EXTRA_COUPON_CODE, couponCode)
            }
        }
    }

    private inner class JavaScriptInterface {
        @JavascriptInterface
        fun dismiss() {
            DengageLogger.verbose("In app message: dismiss event")
            finish()
        }

        @JavascriptInterface
        fun androidUrl(targetUrl: String) {
            if (isAndroidUrlNPresent == false) {
                DengageLogger.verbose("In app message: android target url event $targetUrl")

                if (targetUrl == "Dn.promptPushPermission()") {
                    if (!areNotificationsEnabled()) {
                        Toast.makeText(
                            this@InAppMessageActivity,
                            "You need to enable push permission",
                            Toast.LENGTH_LONG
                        ).show()
                        launchNotificationSettingsActivity()
                    }
                } else {
                    try {
                        launchActivity(null, targetUrl)
                    } catch (e: Exception) {
                        e.printStackTrace()
                    }
                }
            }
        }

        @JavascriptInterface
        fun androidUrlN(targetUrl: String, inAppBrowser: Boolean, retrieveOnSameLink: Boolean) {
            DengageLogger.verbose("In app message: android target url n event $targetUrl")
            when {
                targetUrl.equals("DN.SHOWRATING()", ignoreCase = true) -> showRating()
                targetUrl == "Dn.promptPushPermission()" -> {
                    if (!areNotificationsEnabled()) {
                        Toast.makeText(
                            this@InAppMessageActivity,
                            "You need to enable push permission",
                            Toast.LENGTH_LONG
                        ).show()
                        launchNotificationSettingsActivity()
                    }
                }
                DengageUtils.isDeeplink(targetUrl) -> {
                    try {
                        if (retrieveOnSameLink) {
                            val intent = Intent(Intent.ACTION_VIEW, Uri.parse(targetUrl))
                            intent.putExtra("targetUrl", targetUrl)
                            DengageUtils.sendBroadCast(intent.apply {
                                action = Constants.DEEPLINK_RETRIEVE_EVENT
                            }, applicationContext)
                            intent.extras?.let { setResult(it.getInt(RESULT_CODE), intent) }
                        } else {
                            launchActivity(null, targetUrl)
                        }
                    } catch (e: Exception) {
                        DengageLogger.error(e.message)
                    }
                }
                retrieveOnSameLink && !inAppBrowser -> {
                    val intent = Intent(Intent.ACTION_VIEW, Uri.parse(targetUrl))
                    intent.putExtra("targetUrl", targetUrl)
                    DengageUtils.sendBroadCast(intent.apply {
                        action = Constants.DEEPLINK_RETRIEVE_EVENT
                    }, applicationContext)
                    intent.extras?.let { setResult(it.getInt(RESULT_CODE), intent) }
                }
                inAppBrowser -> {
                    if(targetUrl.isEmpty()) {
                        DengageLogger.error("In app message: target URL is empty")
                        return
                    }
                    val intent = InAppBrowserActivity.Builder
                        .getBuilder()
                        .withUrl(targetUrl)
                        .build(this@InAppMessageActivity)
                    startActivity(intent)
                }
                else -> launchActivity(null, targetUrl)
            }
        }

        @JavascriptInterface
        fun sendClick(buttonId: String?, buttonType: String?) {
            isClicked = true
            DengageLogger.verbose("In app message: clicked button $buttonId $buttonType")
            inAppMessageCallback?.inAppMessageClicked(inAppMessage, buttonId, buttonType)
        }

        @JavascriptInterface
        fun sendClick(buttonId: String?) {
            isClicked = true
            DengageLogger.verbose("In app message: clicked button $buttonId")
            inAppMessageCallback?.inAppMessageClicked(inAppMessage, buttonId, null)
        }

        @JavascriptInterface
        fun sendClick() {
            isClicked = true
            DengageLogger.verbose("In app message: clicked body/button with no Id")
            inAppMessageCallback?.inAppMessageClicked(inAppMessage, null, null)
        }

        @JavascriptInterface
        fun close() {
            if (isAndroidUrlNPresent == false && isRatingDialog == false) {
                DengageLogger.verbose("In app message: close event")
                finish()
            }
        }

        @JavascriptInterface
        fun closeN() {
            DengageLogger.verbose("In app message: close event n")
            if (isRatingDialog == false) {
                finish()
            }
        }

        @JavascriptInterface
        fun setTags() {
            DengageLogger.verbose("In app message: set tags event (no data)")
        }

        @JavascriptInterface
        fun setTags(tagsString: String?) {
            val tagItemString = tagsString?.trim()?.takeIf { it.isNotEmpty() } ?: return
            val components = tagItemString.trim('{', '}')
                .split(",")
                .mapNotNull { component ->
                    val pair = component.split(":").map { it.trim() }
                    if (pair.size == 2) pair[0] to pair[1] else null
                }.toMap()

            val tagItem = TagItem(
                tag = components["tag"] ?: return,
                value = components["value"] ?: return
            )
            inAppMessageCallback?.sendTags(listOf(tagItem))
            DengageLogger.verbose("In app message: set tags event with $tagItem")
        }

        @JavascriptInterface
        fun iosUrl(targetUrl: String) {
            DengageLogger.verbose("In app message: ios target url event $targetUrl")
        }

        @JavascriptInterface
        fun iosUrlN(targetUrl: String, inAppBrowser: Boolean, retrieveOnSameLink: Boolean) {
            DengageLogger.verbose("In app message: ios target url n event $targetUrl")
        }

        @JavascriptInterface
        fun promptPushPermission() {
            DengageLogger.verbose("In app message: prompt push permission event")
            if (!areNotificationsEnabled()) {
                Toast.makeText(
                    this@InAppMessageActivity,
                    "You need to enable push permission",
                    Toast.LENGTH_LONG
                ).show()
                launchNotificationSettingsActivity()
            }
        }

        @JavascriptInterface
        fun openSettings() {
            DengageLogger.verbose("In app message: open settings event")
            launchApplicationSettingsActivity()
        }
    }

    private fun showRating() {
        Dengage.showRatingDialog(
            activity = this@InAppMessageActivity,
            reviewDialogCallback = object : ReviewDialogCallback {
                override fun onCompletion() {}
                override fun onError() {}
            }
        )
        finish()
    }
}






