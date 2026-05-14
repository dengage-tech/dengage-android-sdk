package com.dengage.sdk.ui.story

import android.content.Context
import android.graphics.Typeface
import androidx.core.content.res.ResourcesCompat
import com.dengage.sdk.util.DengageLogger

/**
 * Resolves a font family name to a [Typeface], preferring fonts declared in the host app's
 * `res/font/` directory. Falls back to [Typeface.create] for system family names
 * (e.g. "sans-serif", "monospace") and to the default typeface otherwise.
 *
 * The family name is sanitized to a valid Android resource identifier
 * (lowercase, non-alphanumeric characters replaced with underscore). For example:
 *   "Neue Haas Grotesk Text Pro" → res/font/neue_haas_grotesk_text_pro.(ttf|otf|xml)
 *
 * A CSS-style stack like "Arial, Helvetica, sans-serif" is supported: each candidate
 * is tried in order.
 */
internal object StoryFontResolver {
    private val resourceCache = mutableMapOf<String, Typeface>()
    private val missingResources = mutableSetOf<String>()

    fun resolve(context: Context, familyName: String?, style: Int): Typeface {
        val raw = familyName?.trim()?.takeIf { it.isNotEmpty() }
            ?: return Typeface.defaultFromStyle(style)

        val candidates = raw.split(',')
            .map { it.trim().trim('"', '\'') }
            .filter { it.isNotEmpty() }

        candidates.forEach { candidate ->
            loadFromResources(context, candidate)?.let { base ->
                return if (style != Typeface.NORMAL) Typeface.create(base, style) else base
            }
        }

        val firstCandidate = candidates.firstOrNull() ?: return Typeface.defaultFromStyle(style)
        return Typeface.create(firstCandidate, style)
    }

    private fun loadFromResources(context: Context, name: String): Typeface? {
        val identifiers = listOf(sanitize(name), sanitize(name).replace("_", ""))
            .filter { it.isNotEmpty() }
            .distinct()

        val res = context.resources
        val pkg = context.packageName

        for (id in identifiers) {
            resourceCache[id]?.let { return it }
            if (missingResources.contains(id)) continue

            val resId = try {
                res.getIdentifier(id, "font", pkg)
            } catch (e: Exception) {
                0
            }
            if (resId == 0) {
                missingResources.add(id)
                continue
            }

            val typeface = try {
                ResourcesCompat.getFont(context, resId)
            } catch (e: Exception) {
                DengageLogger.verbose("StoryFontResolver: failed to load res/font/$id: ${e.message}")
                null
            }
            if (typeface != null) {
                resourceCache[id] = typeface
                return typeface
            }
            missingResources.add(id)
        }
        return null
    }

    private fun sanitize(name: String): String =
        name.lowercase()
            .replace(Regex("[^a-z0-9]+"), "_")
            .trim('_')
}
