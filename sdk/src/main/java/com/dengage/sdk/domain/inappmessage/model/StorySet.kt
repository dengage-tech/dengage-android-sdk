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

data class StorySetStyling(
    @SerializedName("fontFamily") val fontFamily: String = "",
    @SerializedName("mobileOverlayColor") val mobileOverlayColor: String = "#fff",
    @SerializedName("headerTitle") val headerTitle: StorySetHeaderTitle = StorySetHeaderTitle(),
    @SerializedName("headerCover") val headerCover: StorySetHeaderCover = StorySetHeaderCover()
): Serializable {
    val mobileOverlayColorInt: Int
        get() = parseColor(mobileOverlayColor)
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
    @SerializedName("stories") val stories: List<Story> = listOf(),
    @SerializedName("shown") var shown: Boolean? = false
) : Serializable {
}

data class Story(
    @SerializedName("id") val id: String = "",
    @SerializedName("name") val name: String = "",
    @SerializedName("mediaUrl") val mediaUrl: String? = null,
    @SerializedName("type") val type: String? = null,
    @SerializedName("bgColors") val bgColors: List<String>? = null,
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
