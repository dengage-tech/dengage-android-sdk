package com.dengage.sdk.ui

import android.annotation.SuppressLint
import android.content.Context
import android.content.res.Resources
import android.graphics.Color
import android.util.AttributeSet
import android.view.View
import android.view.ViewGroup
import android.webkit.WebView
import android.widget.RelativeLayout
import androidx.cardview.widget.CardView
import androidx.constraintlayout.widget.ConstraintLayout
import com.dengage.sdk.R
import com.dengage.sdk.domain.inappmessage.model.ContentParams
import com.dengage.sdk.domain.inappmessage.model.ContentPosition
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.manager.inappmessage.util.InAppMessageUtils
import kotlin.math.roundToInt

class InAppInlineElement : WebView {
    private var contextWebView :Context?=null
    constructor(context: Context?) : super(context!!){
        contextWebView=context
    }
    constructor(context: Context?, attrs: AttributeSet?) : super(
        context!!, attrs) {
        contextWebView=context
    }
  fun populateInLineInApp(inAppMessage: InAppMessage){
      val contentParams = inAppMessage.data.content.params
      setContentPosition(contentParams)
      setHtmlContent(contentParams)

  }


    private fun setContentPosition(
        contentParams: ContentParams,
    ) {
        val cardInAppMessage = findViewById<CardView>(R.id.cardInAppMessage)
        val params = RelativeLayout.LayoutParams(
            ViewGroup.LayoutParams.WRAP_CONTENT,
            ViewGroup.LayoutParams.WRAP_CONTENT
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
                ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.MATCH_PARENT
            )
            webView.layoutParams = params
        }

        // set radius of card view
        cardInAppMessage.radius = InAppMessageUtils.pxToDp(contentParams.radius, contextWebView!! )

        // set max width for container
        contentParams.maxWidth?.let {
            val params = vHtmlWidthContainer.layoutParams as ConstraintLayout.LayoutParams
            params.matchConstraintMaxWidth = InAppMessageUtils.pxToDp(it, contextWebView!! ).roundToInt()
            vHtmlWidthContainer.layoutParams = params
        }

        vHtmlContent.visibility = View.VISIBLE


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
           // addJavascriptInterface(JavaScriptInterface(), "Dn")
        }
    }

}