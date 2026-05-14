package com.dengage.sdk.domain.inappmessage.model

import android.graphics.Color
import com.google.gson.*
import com.google.gson.annotations.SerializedName
import java.io.Serializable
import java.util.*

enum class StorySetFontWeight: Serializable {
    @SerializedName("normal") NORMAL,
    @SerializedName("bold") BOLD
}

enum class StorySetImagePositioning(val rawValue: String): Serializable {
    @SerializedName("fit") FIT("fit"),
    @SerializedName("fill") FILL("fill");

    companion object {
        fun from(value: String?): StorySetImagePositioning? = when (value?.lowercase()) {
            FIT.rawValue -> FIT
            FILL.rawValue -> FILL
            else -> null
        }
    }
}

data class StorySetHeaderTitle(
    @SerializedName("textColor") val textColor: String = "#1C2C48",
    @SerializedName("textAlign") val textAlign: String = "left",
    @SerializedName("fontSize") val fontSize: Int = 16,
    @SerializedName("fontWeight") val fontWeight: StorySetFontWeight = StorySetFontWeight.NORMAL
): Serializable {
    val textColorInt: Int
        get() = parseColor(textColor)

    val textAlignment: Int
        get() = when (textAlign.lowercase()) {
            "right" -> android.view.Gravity.END
            "center" -> android.view.Gravity.CENTER
            else -> android.view.Gravity.START
        }
}

data class StorySetHeaderCover(
    @SerializedName("size") val size: Int = 80,
    @SerializedName("gap") val gap: Int = 8,
    @SerializedName("textColor") val textColor: String = "#1C2C48",
    @SerializedName("fontSize") val fontSize: Int = 12,
    @SerializedName("fontWeight") val fontWeight: StorySetFontWeight = StorySetFontWeight.NORMAL,
    @SerializedName("borderRadius") val borderRadius: String = "50%",
    @SerializedName("borderWidth") val borderWidth: Int = 6,
    @SerializedName("fillerAngle") val fillerAngle: Int = 45,
    @SerializedName("fillerColors") val fillerColors: List<String> = listOf("#f09433", "#dc2743"),
    @SerializedName("passiveColor") val passiveColor: String = "#A0A0AD"
): Serializable {
    val textColorInt: Int
        get() = parseColor(textColor)

    val fillerColorsInt: List<Int>
        get() = fillerColors.map { parseColor(it) }

    val passiveColorInt: Int
        get() = parseColor(passiveColor)

    val borderRadiusDouble: Double
        get() = borderRadius.replace("%", "").toDoubleOrNull()?.div(100) ?: 0.5
}

data class StorySetPadding(
    @SerializedName("left") val left: Int = 0,
    @SerializedName("top") val top: Int = 0,
    @SerializedName("right") val right: Int = 0,
    @SerializedName("bottom") val bottom: Int = 0
): Serializable

data class StorySetButtonTitle(
    @SerializedName("fontFamily") val fontFamily: String? = null,
    @SerializedName("fontWeight") val fontWeight: StorySetFontWeight? = null,
    @SerializedName("textAlign") val textAlign: String? = null,
    @SerializedName("fontSize") val fontSize: Int? = null,
    @SerializedName("fontColor") val fontColor: String? = null
): Serializable

data class StorySetButton(
    @SerializedName("backgroundColor") val backgroundColor: String? = null,
    @SerializedName("borderColor") val borderColor: String? = null,
    @SerializedName("borderRadius") val borderRadius: Int? = null,
    @SerializedName("height") val height: Int? = null,
    @SerializedName("padding") val padding: StorySetPadding? = null,
    @SerializedName("fitContent") val fitContent: Boolean? = null
): Serializable

data class StorySetDark(
    @SerializedName("storyBackgroundColor") val storyBackgroundColor: String? = null,
    @SerializedName("buttonTitle") val buttonTitle: StorySetButtonTitle? = null,
    @SerializedName("button") val button: StorySetButton? = null
): Serializable

data class StorySetStyling(
    @SerializedName("fontFamily") val fontFamily: String = "",
    @SerializedName("mobileOverlayColor") val mobileOverlayColor: String = "#fff",
    @SerializedName("iosFontFamily") val iosFontFamily: String? = null,
    @SerializedName("androidFontFamily") val androidFontFamily: String? = null,
    @SerializedName("headerTitle") val headerTitle: StorySetHeaderTitle = StorySetHeaderTitle(),
    @SerializedName("headerCover") val headerCover: StorySetHeaderCover = StorySetHeaderCover(),
    @SerializedName("storyBackgroundColor") val storyBackgroundColor: String? = null,
    @SerializedName("buttonTitle") val buttonTitle: StorySetButtonTitle? = null,
    @SerializedName("button") val button: StorySetButton? = null,
    @SerializedName("dark") val dark: StorySetDark? = null
): Serializable {
    val mobileOverlayColorInt: Int
        get() = parseColor(mobileOverlayColor)

    /** Per migration guide: androidFontFamily > legacy fontFamily. */
    val effectiveFontFamily: String?
        get() = androidFontFamily?.takeIf { it.isNotBlank() }
            ?: fontFamily.takeIf { it.isNotBlank() }

    /**
     * Resolved story background color following fallback chain:
     * dark.storyBackgroundColor (when dark) -> styling.storyBackgroundColor -> story.bgColors[0].
     * Returns null when no value is configured; caller should fall back to a sensible default.
     */
    fun resolvedStoryBackgroundColor(story: Story, isDarkMode: Boolean): String? {
        if (isDarkMode) {
            dark?.storyBackgroundColor?.takeIf { it.isNotBlank() }?.let { return it }
        }
        storyBackgroundColor?.takeIf { it.isNotBlank() }?.let { return it }
        return story.bgColors?.firstOrNull()?.takeIf { it.isNotBlank() }
    }

    /** dark.buttonTitle.fontColor -> styling.buttonTitle.fontColor -> cta.textColor. */
    fun resolvedButtonTextColor(cta: StoryCta?, isDarkMode: Boolean): String? {
        if (isDarkMode) {
            dark?.buttonTitle?.fontColor?.takeIf { it.isNotBlank() }?.let { return it }
        }
        buttonTitle?.fontColor?.takeIf { it.isNotBlank() }?.let { return it }
        return cta?.bgColor?.let { cta.textColor.takeIf { c -> c.isNotBlank() } }
    }

    /** dark.button.backgroundColor -> styling.button.backgroundColor -> cta.bgColor. */
    fun resolvedButtonBackgroundColor(cta: StoryCta?, isDarkMode: Boolean): String? {
        if (isDarkMode) {
            dark?.button?.backgroundColor?.takeIf { it.isNotBlank() }?.let { return it }
        }
        button?.backgroundColor?.takeIf { it.isNotBlank() }?.let { return it }
        return cta?.bgColor?.takeIf { it.isNotBlank() }
    }

    fun resolvedButtonBorderColor(isDarkMode: Boolean): String? {
        if (isDarkMode) {
            dark?.button?.borderColor?.takeIf { it.isNotBlank() }?.let { return it }
        }
        return button?.borderColor?.takeIf { it.isNotBlank() }
    }

    fun resolvedButton(isDarkMode: Boolean): StorySetButton? =
        if (isDarkMode) dark?.button ?: button else button

    fun resolvedButtonTitle(isDarkMode: Boolean): StorySetButtonTitle? =
        if (isDarkMode) dark?.buttonTitle ?: buttonTitle else buttonTitle

    /** Pick an int field from dark.button then light button. */
    fun resolvedButtonInt(isDarkMode: Boolean, selector: (StorySetButton) -> Int?): Int? {
        if (isDarkMode) {
            dark?.button?.let(selector)?.let { return it }
        }
        return button?.let(selector)
    }

    /** Pick a value from dark.buttonTitle then light buttonTitle. */
    fun <T : Any> resolvedButtonTitleField(isDarkMode: Boolean, selector: (StorySetButtonTitle) -> T?): T? {
        if (isDarkMode) {
            dark?.buttonTitle?.let(selector)?.let { return it }
        }
        return buttonTitle?.let(selector)
    }
}

data class StorySet(
    @SerializedName("id") val id: String = UUID.randomUUID().toString(),
    @SerializedName("title") val title: String = "",
    @SerializedName("styling") val styling: StorySetStyling = StorySetStyling(),
    @SerializedName("covers") var covers: List<StoryCover> = listOf(),
): Serializable {
    fun copy(): StorySet {
        val gson = Gson()
        val json = gson.toJson(this)
        return gson.fromJson(json, StorySet::class.java)
    }
}

data class StoryCover(
    @SerializedName("id") val id: String = "",
    @SerializedName("name") val name: String = "",
    @SerializedName("mediaUrl") val mediaUrl: String = "",
    @SerializedName("imagePositioning") val imagePositioning: String? = null,
    @SerializedName("stories") val stories: List<Story> = listOf(),
    @SerializedName("shown") var shown: Boolean? = false
) : Serializable {
    val imagePositioningEnum: StorySetImagePositioning?
        get() = StorySetImagePositioning.from(imagePositioning)
}

data class Story(
    @SerializedName("id") val id: String = "",
    @SerializedName("name") val name: String = "",
    @SerializedName("mediaUrl") val mediaUrl: String? = null,
    @SerializedName("type") val type: String? = null,
    @SerializedName("bgColors") val bgColors: List<String>? = null,
    @SerializedName("imagePositioning") val imagePositioning: String? = null,
    @SerializedName("duration") val duration: Int? = null,
    @SerializedName("cta") val cta: StoryCta? = null
): Serializable {
    val gradColors: List<Int>
        get() = bgColors?.map { parseColor(it) }?.take(2) ?: listOf()

    val kind: MimeType
        get() = when (type) {
            MimeType.IMAGE.rawValue -> MimeType.IMAGE
            MimeType.VIDEO.rawValue -> MimeType.VIDEO
            else -> MimeType.UNKNOWN
        }

    val imagePositioningEnum: StorySetImagePositioning?
        get() = StorySetImagePositioning.from(imagePositioning)
}

data class StoryCta(
    @SerializedName("isEnabled") val isEnabled: Boolean = false,
    @SerializedName("androidLink") val androidLink: String = "",
    @SerializedName("label") val label: String = "",
    @SerializedName("bgColor") val bgColor: String = "",
    @SerializedName("textColor") val textColor: String = ""
): Serializable {
    val bgColorInt: Int
        get() = parseColor(bgColor)

    val textColorInt: Int
        get() = parseColor(textColor)
}

enum class MimeType(val rawValue: String): Serializable {
    @SerializedName("image") IMAGE("image"),
    @SerializedName("video") VIDEO("video"),
    UNKNOWN("unknown")
}

fun parseColor(hex: String?): Int {
    hex ?: return Color.BLACK

    val color = when (hex.length) {
        4 -> {
            // Convert #RGB to #RRGGBB
            "#" + hex[1] + hex[1] + hex[2] + hex[2] + hex[3] + hex[3]
        }
        7 -> hex
        9 -> {
            // Convert #RGB to #RRGGBB
            hex.take(7)
        }
        else -> return Color.BLACK
    }

    return try {
        Color.parseColor(color)
    } catch (e: IllegalArgumentException) {
        Color.BLACK
    }
}
