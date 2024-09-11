package com.dengage.sdk.ui.story

import android.content.Context
import android.graphics.Color
import android.graphics.Typeface
import android.util.AttributeSet
import android.view.View
import android.widget.LinearLayout
import android.widget.TextView
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.usecase.StoryEventType
import com.dengage.sdk.util.ContextHolder
import com.dengage.sdk.domain.inappmessage.model.StoryCover
import com.dengage.sdk.domain.inappmessage.model.StorySetFontWeight
import com.google.android.exoplayer2.database.DatabaseProvider
import com.google.android.exoplayer2.database.ExoDatabaseProvider
import com.google.android.exoplayer2.upstream.cache.Cache
import com.google.android.exoplayer2.upstream.cache.LeastRecentlyUsedCacheEvictor
import com.google.android.exoplayer2.upstream.cache.SimpleCache

class StoriesListView : LinearLayout {
    private var titleView: TextView? = null

    constructor(context: Context?) : super(context!!) {
        init()
    }
    constructor(context: Context?, attrs: AttributeSet?) : super(context!!, attrs) {
        init()
    }
    constructor(context: Context?, attrs: AttributeSet?, defStyle: Int) : super(
        context!!,
        attrs,
        defStyle
    ) {
        init()
    }

    private fun init() {
        orientation = VERTICAL
        titleView = TextView(context).apply {
            visibility = View.GONE
            textSize = 16f
            setTextColor(Color.BLACK)
            setPadding(8, 8, 8, 8)
            layoutParams = LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT).apply {
                setMargins(8, 8, 8, 8)
            }
        }
        recyclerView = RecyclerView(context).apply {
            layoutManager = LinearLayoutManager(context, LinearLayoutManager.HORIZONTAL, false)
            layoutParams = LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT).apply {
                setMargins(0, 8, 0, 8)
            }
        }
        addView(titleView)
        addView(recyclerView)
    }

    internal fun loadInAppMessage(inAppMessage: InAppMessage, publicId: String, contentId: String) {
        val title = inAppMessage.data.content.params.storySet?.title
        val styling = inAppMessage.data.content.params.storySet?.styling
        if (!title.isNullOrEmpty()) {
            titleView?.text = title
            titleView?.visibility = View.VISIBLE
        } else {
            titleView?.visibility = View.GONE
        }
        styling?.headerTitle?.let {
            titleView?.setTextColor(it.textColorInt)
            titleView?.textSize = it.fontSize.toFloat()
            titleView?.gravity = it.textAlignment
            if(it.fontWeight == StorySetFontWeight.BOLD) {
                titleView?.typeface = Typeface.DEFAULT_BOLD
            } else {
                titleView?.typeface = Typeface.DEFAULT
            }
        }
        val storiesListAdapter = StoriesListAdapter(context, inAppMessage, publicId, contentId)
        storiesListAdapter.setStoryList()
        recyclerView?.adapter = storiesListAdapter
    }

    companion object {
        const val TAG = "StoriesListView"
        var inAppMessageCallback: InAppMessageCallback? = null
        var recyclerView: RecyclerView? = null
        private val leastRecentlyUsedCacheEvictor = LeastRecentlyUsedCacheEvictor(90 * 1024 * 1024)
        private val databaseProvider: DatabaseProvider = ExoDatabaseProvider(ContextHolder.context)
        var simpleCache: Cache = SimpleCache(
            ContextHolder.context.cacheDir,
            leastRecentlyUsedCacheEvictor,
            databaseProvider
        )
    }

    interface InAppMessageCallback {
        fun storyEvent(
            eventType: StoryEventType,
            inAppMessage: InAppMessage,
            storyProfileId: String,
            storyProfileName: String,
            storyId: String,
            storyName: String,
            buttonUrl: String = ""
        )

        fun setStoryCoverShown(storyCoverId: String, storySetId: String)
        fun sortStoryCovers(storyCovers: List<StoryCover>, storySetId: String): List<StoryCover>
    }
}
