package com.dengage.sdk.util

import android.app.Activity
import android.graphics.Color
import android.os.Build
import android.view.View
import android.view.WindowInsets
import android.view.WindowInsetsController
import androidx.core.view.ViewCompat
import androidx.core.view.WindowCompat
import androidx.core.view.WindowInsetsCompat
import androidx.core.view.WindowInsetsControllerCompat

/**
 * Utility class for handling edge-to-edge display in Android 15
 * Provides methods to enable immersive full-screen experience for stories
 */
object EdgeToEdgeUtils {

    /**
     * Enable edge-to-edge display for an activity
     * This method should be called in onCreate() before setContentView()
     */
    fun enableEdgeToEdge(activity: Activity) {
        // Enable edge-to-edge display
        WindowCompat.setDecorFitsSystemWindows(activity.window, false)
        
        // Set status bar color to primary color
        setStatusBarColor(activity)
        
        // Keep system bars visible but allow content to extend behind them
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            // Use a post to ensure the window is fully created
            activity.window.decorView.post {
                activity.window.insetsController?.let { controller ->
                    // Show system bars and set behavior to show transient bars by swipe
                    controller.show(WindowInsets.Type.systemBars())
                    controller.systemBarsBehavior = WindowInsetsController.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE
                }
            }
        } else {
            @Suppress("DEPRECATION")
            activity.window.decorView.systemUiVisibility = (
                View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                or View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                or View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
            )
        }
    }

    /**
     * Enable edge-to-edge display for an activity with proper insets handling
     * This method should be called in onCreate() after setContentView()
     */
    fun enableEdgeToEdgeWithInsets(activity: Activity) {
        // Enable edge-to-edge display
        WindowCompat.setDecorFitsSystemWindows(activity.window, false)
        
        // Set status bar color to primary color
        setStatusBarColor(activity)
        
        // Setup insets handling for the activity
        setupActivityEdgeToEdgeInsets(activity)
        
        // Keep system bars visible but allow content to extend behind them
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            // Use a post to ensure the window is fully created
            activity.window.decorView.post {
                activity.window.insetsController?.let { controller ->
                    // Show system bars and set behavior to show transient bars by swipe
                    controller.show(WindowInsets.Type.systemBars())
                    controller.systemBarsBehavior = WindowInsetsController.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE
                }
            }
        } else {
            @Suppress("DEPRECATION")
            activity.window.decorView.systemUiVisibility = (
                View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                or View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                or View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
            )
        }
    }

    /**
     * Enable immersive edge-to-edge display for story activities
     * This method hides system bars for full immersive experience
     */
    fun enableImmersiveEdgeToEdge(activity: Activity) {
        // Enable edge-to-edge display
        WindowCompat.setDecorFitsSystemWindows(activity.window, false)
        
        // Hide system bars for immersive experience
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            // Use a post to ensure the window is fully created
            activity.window.decorView.post {
                activity.window.insetsController?.let { controller ->
                    controller.hide(WindowInsets.Type.statusBars() or WindowInsets.Type.navigationBars())
                    controller.systemBarsBehavior = WindowInsetsController.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE
                }
            }
        } else {
            @Suppress("DEPRECATION")
            activity.window.decorView.systemUiVisibility = (
                View.SYSTEM_UI_FLAG_FULLSCREEN
                or View.SYSTEM_UI_FLAG_HIDE_NAVIGATION
                or View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY
                or View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                or View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                or View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
            )
        }
    }

    /**
     * Setup edge-to-edge insets handling for a view
     * This method should be called in onViewCreated() for fragments
     */
    fun setupEdgeToEdgeInsets(view: View) {
        ViewCompat.setOnApplyWindowInsetsListener(view) { v, insets ->
            val systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars())
            val displayCutout = insets.getInsets(WindowInsetsCompat.Type.displayCutout())

            // Apply padding to handle system bars and display cutout
            v.setPadding(
                maxOf(systemBars.left, displayCutout.left),
                maxOf(systemBars.top, displayCutout.top),
                maxOf(systemBars.right, displayCutout.right),
                maxOf(systemBars.bottom, displayCutout.bottom)
            )

            // Return the insets to continue the chain
            insets
        }
    }

    /**
     * Setup edge-to-edge insets handling for activity root view
     * This method should be called after setContentView() in activities
     */
    fun setupActivityEdgeToEdgeInsets(activity: Activity) {
        val rootView = activity.findViewById<View>(android.R.id.content)
        ViewCompat.setOnApplyWindowInsetsListener(rootView) { v, insets ->
            val systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars())
            val displayCutout = insets.getInsets(WindowInsetsCompat.Type.displayCutout())

            // Apply padding to handle system bars and display cutout
            v.setPadding(
                maxOf(systemBars.left, displayCutout.left),
                maxOf(systemBars.top, displayCutout.top),
                maxOf(systemBars.right, displayCutout.right),
                maxOf(systemBars.bottom, displayCutout.bottom)
            )

            // Return the insets to continue the chain
            insets
        }
    }

    /**
     * Setup edge-to-edge insets handling for a view with custom padding
     * This method allows for custom padding values while still handling insets
     */
    fun setupEdgeToEdgeInsetsWithCustomPadding(
        view: View,
        customPadding: (systemBars: androidx.core.graphics.Insets, displayCutout: androidx.core.graphics.Insets) -> androidx.core.graphics.Insets
    ) {
        ViewCompat.setOnApplyWindowInsetsListener(view) { v, insets ->
            val systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars())
            val displayCutout = insets.getInsets(WindowInsetsCompat.Type.displayCutout())
            
            val padding = customPadding(systemBars, displayCutout)
            v.setPadding(padding.left, padding.top, padding.right, padding.bottom)
            
            insets
        }
    }

    /**
     * Show system bars temporarily (for user interaction)
     */
    fun showSystemBarsTemporarily(activity: Activity) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            activity.window.decorView.post {
                activity.window.insetsController?.show(WindowInsets.Type.systemBars())
            }
        } else {
            @Suppress("DEPRECATION")
            activity.window.decorView.systemUiVisibility = (
                View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                or View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                or View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
            )
        }
    }

    /**
     * Hide system bars again after showing them temporarily
     */
    fun hideSystemBars(activity: Activity) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            activity.window.decorView.post {
                activity.window.insetsController?.hide(WindowInsets.Type.systemBars())
            }
        } else {
            @Suppress("DEPRECATION")
            activity.window.decorView.systemUiVisibility = (
                View.SYSTEM_UI_FLAG_FULLSCREEN
                or View.SYSTEM_UI_FLAG_HIDE_NAVIGATION
                or View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY
                or View.SYSTEM_UI_FLAG_LAYOUT_STABLE
                or View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION
                or View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN
            )
        }
    }

    /**
     * Check if the device has a display cutout (notch)
     */
    fun hasDisplayCutout(activity: Activity): Boolean {
        return if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
            activity.window.decorView.rootWindowInsets?.displayCutout != null
        } else {
            false
        }
    }

    /**
     * Get the safe area insets for the current activity
     */
    fun getSafeAreaInsets(activity: Activity): androidx.core.graphics.Insets {
        val insets = ViewCompat.getRootWindowInsets(activity.window.decorView)
        return insets?.getInsets(WindowInsetsCompat.Type.systemBars()) ?: androidx.core.graphics.Insets.NONE
    }

    /**
     * Set status bar color to primary color
     */
    private fun setStatusBarColor(activity: Activity) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
            // Get primary color from theme
            val typedArray = activity.theme.obtainStyledAttributes(intArrayOf(android.R.attr.colorPrimary))
            val primaryColor = typedArray.getColor(0, Color.BLUE) // Default to blue if not found
            typedArray.recycle()
            
            // Set status bar color
            activity.window.statusBarColor = primaryColor
            
            // Set status bar content to light if primary color is dark
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
                val isLightColor = isColorLight(primaryColor)
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                    // Use modern API for Android 11+
                    activity.window.decorView.post {
                        activity.window.insetsController?.let { controller ->
                            if (isLightColor) {
                                controller.setSystemBarsAppearance(0, WindowInsetsController.APPEARANCE_LIGHT_STATUS_BARS)
                            } else {
                                controller.setSystemBarsAppearance(
                                    WindowInsetsController.APPEARANCE_LIGHT_STATUS_BARS,
                                    WindowInsetsController.APPEARANCE_LIGHT_STATUS_BARS
                                )
                            }
                        }
                    }
                } else {
                    // Use deprecated API for older versions
                    @Suppress("DEPRECATION")
                    activity.window.decorView.systemUiVisibility = if (isLightColor) {
                        activity.window.decorView.systemUiVisibility and View.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR.inv()
                    } else {
                        activity.window.decorView.systemUiVisibility or View.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR
                    }
                }
            }
        }
    }

    /**
     * Set status bar icons to dark/grey without enabling edge-to-edge
     * This is useful for activities that don't want edge-to-edge but need dark status bar icons
     */
    fun setStatusBarIconsToDark(activity: Activity) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                // Use modern API for Android 11+
                activity.window.decorView.post {
                    activity.window.insetsController?.let { controller ->
                        // Set status bar icons to dark (light background with dark icons)
                        controller.setSystemBarsAppearance(0, WindowInsetsController.APPEARANCE_LIGHT_STATUS_BARS)
                    }
                }
            } else {
                // Use deprecated API for older versions
                @Suppress("DEPRECATION")
                activity.window.decorView.systemUiVisibility = 
                    activity.window.decorView.systemUiVisibility and View.SYSTEM_UI_FLAG_LIGHT_STATUS_BAR.inv()
            }
        }
    }

    /**
     * Check if a color is light (for determining status bar content color)
     */
    private fun isColorLight(color: Int): Boolean {
        val darkness = 1 - (0.299 * Color.red(color) + 0.587 * Color.green(color) + 0.114 * Color.blue(color)) / 255
        return darkness < 0.5
    }
}
