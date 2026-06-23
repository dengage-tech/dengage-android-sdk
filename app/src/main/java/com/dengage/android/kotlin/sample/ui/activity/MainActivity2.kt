package com.dengage.android.kotlin.sample.ui.activity

import android.content.Intent
import androidx.appcompat.app.AppCompatActivity
import android.net.Uri
import android.os.Bundle
import android.util.Log
import android.widget.TextView
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.ui.fragment.RecommendationOrderFragment
import com.dengage.sdk.Dengage
import com.dengage.sdk.util.EdgeToEdgeUtils

/**
 * Landing activity for sample app deeplinks (scheme: "dengagesample://").
 *
 * When a link inside a RecommendationView points to a deeplink, the SDK fires an
 * ACTION_VIEW intent that the system routes here (see the intent-filter for this
 * activity in AndroidManifest.xml). The received URI is parsed and shown on screen.
 */
class MainActivity2 : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        setContentView(R.layout.activity_main2)

        // Enable edge-to-edge display for Android 15 with proper insets handling
        EdgeToEdgeUtils.enableEdgeToEdgeWithInsets(this)

        Dengage.setCurrentActivity(this)
        Dengage.restartApplicationAfterPushClick(true)

        handleDeeplink(intent)
    }

    override fun onNewIntent(intent: Intent?) {
        super.onNewIntent(intent)
        // singleTop: reuse this instance when another deeplink arrives.
        setIntent(intent)
        handleDeeplink(intent)
    }

    private fun handleDeeplink(intent: Intent?) {
        val uri: Uri? = intent?.data
        Log.d("oops", "Deeplink received: $uri")

        val urlView = findViewById<TextView>(R.id.tvDeeplinkUrl)
        val paramsView = findViewById<TextView>(R.id.tvDeeplinkParams)

        if (uri == null) {
            urlView.text = "No deeplink data in intent."
            paramsView.text = ""
            return
        }

        urlView.text = uri.toString()

        // Route specific deeplinks to their own fragment.
        when (uri.host) {
            "recommendationOrder" -> {
                val productId = uri.getQueryParameter("productId").orEmpty()
                supportFragmentManager.beginTransaction()
                    .replace(
                        R.id.fragmentContainer,
                        RecommendationOrderFragment.newInstance(productId)
                    )
                    .commit()
            }
            else -> {
                // No dedicated fragment for this deeplink; clear any previous one.
                supportFragmentManager.findFragmentById(R.id.fragmentContainer)?.let {
                    supportFragmentManager.beginTransaction().remove(it).commit()
                }
            }
        }

        val details = buildString {
            append("scheme: ").append(uri.scheme).append('\n')
            append("host: ").append(uri.host ?: "-").append('\n')
            append("path: ").append(uri.path?.ifEmpty { "-" } ?: "-")
            val params = uri.queryParameterNames
            if (params.isNotEmpty()) {
                append("\n\nQuery parameters:")
                params.forEach { name ->
                    append('\n').append(name).append(": ").append(uri.getQueryParameter(name))
                }
            }
        }
        paramsView.text = details
    }
}
