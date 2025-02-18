package com.dengage.sdk.ui.inappmessage

import com.dengage.sdk.util.DengageLogger

object Mustache {

    fun render(template: String, view: Any?): String {
        return try {
            val (tokens, _) = parseTokens(template, 0, stopTag = null)
            val context = Context(view)
            renderTokens(tokens, context)
        } catch (e: Exception) {
            DengageLogger.error("Mustache template rendering error: ${e.message}")
            template
        }
    }

    private fun parseTokens(
        template: String,
        startPos: Int,
        stopTag: String?
    ): Pair<List<Token>, Int> {
        val tokens = mutableListOf<Token>()
        var pos = startPos
        while (pos < template.length) {
            val tagStart = template.indexOf("{{", pos)
            if (tagStart == -1) {
                tokens.add(Token.Text(template.substring(pos)))
                pos = template.length
                break
            }
            if (tagStart > pos) {
                tokens.add(Token.Text(template.substring(pos, tagStart)))
            }
            val tagEnd = template.indexOf("}}", tagStart)
            if (tagEnd == -1) {
                throw IllegalArgumentException("Tag not closed")
            }
            val tagContent = template.substring(tagStart + 2, tagEnd).trim()
            pos = tagEnd + 2

            when {
                tagContent.startsWith("/") -> {
                    val tagName = tagContent.substring(1).trim()
                    if (stopTag != null && tagName == stopTag) {
                        return Pair(tokens, pos)
                    } else {
                        throw IllegalArgumentException("Unexpected closing tag: $tagName")
                    }
                }
                tagContent.startsWith("#") -> {
                    val tagName = tagContent.substring(1).trim()
                    val (sectionTokens, newPos) = parseTokens(template, pos, stopTag = tagName)
                    pos = newPos
                    tokens.add(Token.Section(tagName, sectionTokens))
                }
                tagContent.startsWith("^") -> {
                    val tagName = tagContent.substring(1).trim()
                    val (sectionTokens, newPos) = parseTokens(template, pos, stopTag = tagName)
                    pos = newPos
                    tokens.add(Token.InvertedSection(tagName, sectionTokens))
                }
                tagContent.startsWith("{") && tagContent.endsWith("}") -> {
                    val tagName = tagContent.substring(1, tagContent.length - 1).trim()
                    tokens.add(Token.Unescaped(tagName))
                }
                tagContent.startsWith("&") -> {
                    val tagName = tagContent.substring(1).trim()
                    tokens.add(Token.Unescaped(tagName))
                }
                else -> {
                    tokens.add(Token.Variable(tagContent))
                }
            }
        }
        if (stopTag != null) {
            throw IllegalArgumentException("Unclosed section: $stopTag")
        }
        return Pair(tokens, pos)
    }

    private fun renderTokens(tokens: List<Token>, context: Context): String {
        val sb = StringBuilder()
        for (token in tokens) {
            when (token) {
                is Token.Text -> sb.append(token.text)
                is Token.Variable -> {
                    val value = context.lookup(token.key)
                    if (value != null) {
                        sb.append(escapeHtml(value.toString()))
                    }
                }
                is Token.Unescaped -> {
                    val value = context.lookup(token.key)
                    if (value != null) {
                        sb.append(value.toString())
                    }
                }
                is Token.Section -> {
                    val value = context.lookup(token.key)
                    when (value) {
                        is List<*> -> {
                            for (item in value) {
                                val newContext = context.push(item)
                                sb.append(renderTokens(token.tokens, newContext))
                            }
                        }
                        else -> {
                            if (isTruthy(value)) {
                                val newContext = context.push(value)
                                sb.append(renderTokens(token.tokens, newContext))
                            }
                        }
                    }
                }
                is Token.InvertedSection -> {
                    val value = context.lookup(token.key)
                    if (!isTruthy(value)) {
                        sb.append(renderTokens(token.tokens, context))
                    }
                }
            }
        }
        return sb.toString()
    }

    private fun isTruthy(value: Any?): Boolean =
        when (value) {
            null -> false
            is Boolean -> value
            is List<*> -> value.isNotEmpty()
            is String -> value.isNotEmpty()
            else -> true
        }

    private fun escapeHtml(input: String): String {
        return input.replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace("\"", "&quot;")
            .replace("'", "&#39;")
    }

    private sealed class Token {
        data class Text(val text: String) : Token()
        data class Variable(val key: String) : Token()
        data class Unescaped(val key: String) : Token()
        data class Section(val key: String, val tokens: List<Token>) : Token()
        data class InvertedSection(val key: String, val tokens: List<Token>) : Token()
    }

    class Context(private val view: Any?, private val parent: Context? = null) {

        fun push(newView: Any?): Context = Context(newView, this)

        fun lookup(key: String): Any? {
            if (key == ".") return view
            val parts = key.split(".")
            var value: Any? = view
            for (part in parts) {
                value = when (value) {
                    is Map<*, *> -> value[part]
                    else -> null
                }
                if (value == null) break
            }
            return value ?: parent?.lookup(key)
        }
    }
}
