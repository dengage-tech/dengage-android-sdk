package com.dengage.sdk.ui.story

import android.content.Context
import android.content.Intent
import android.graphics.Color
import android.util.Log
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView
import com.bumptech.glide.Glide
import com.dengage.sdk.R
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.model.StorySet
import com.dengage.sdk.util.Constants

class StoriesListAdapter(
    var context: Context,
    val inAppMessage: InAppMessage,
    val publicId: String,
    val contentId: String
) :
    RecyclerView.Adapter<StoriesListAdapter.StoryHolder>() {
    private var mRecyclerView: RecyclerView? = null
    private var isFirstRun = true

    val storySet: StorySet
        get() = inAppMessage.data.content.params.storySet ?: StorySet()


    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): StoryHolder {
        if (isFirstRun) {
            cacheImagesBeforeDisplaying()
        }
        val context = parent.context
        val inflater = LayoutInflater.from(context)
        val view: View = inflater.inflate(R.layout.story_item, parent, false)
        return StoryHolder(view)
    }

    override fun onAttachedToRecyclerView(recyclerView: RecyclerView) {
        super.onAttachedToRecyclerView(recyclerView)
        mRecyclerView = recyclerView
    }

    override fun onBindViewHolder(storyHolder: StoryHolder, storyCoverPosition: Int) {
        val cover = storySet.covers[storyCoverPosition]
        val shown = cover.shown ?: false
        storyHolder.tvStoryName.text = cover.name
        storyHolder.tvStoryName.setTextColor(storySet.styling.headerCover.textColorInt)
        storyHolder.tvStoryName.textSize = storySet.styling.headerCover.fontSize.toFloat()
        if (cover.mediaUrl != "") {
            storyHolder.civStory.cornerRadiusRatio = storySet.styling.headerCover.borderRadiusDouble.toFloat()
            if(storySet.styling.headerCover.borderRadiusDouble == 0.5) {
                Glide.with(context).load(cover.mediaUrl).circleCrop().into(storyHolder.civStory)
            } else {
                Glide.with(context).load(cover.mediaUrl).centerInside().into(storyHolder.civStory)
            }
        }
        storyHolder.civStory.setOnClickListener { clickEvent(storyCoverPosition) }
        storyHolder.setCircleViewProperties(shown)
    }

    private fun clickEvent(storyCoverPosition: Int) {
        val storyActivity = StoryActivity()
        StoryActivity.storiesListAdapter = this
        val intent = Intent(context, storyActivity.javaClass)
        intent.flags = Intent.FLAG_ACTIVITY_NEW_TASK
        intent.flags = intent.flags or Intent.FLAG_ACTIVITY_NO_HISTORY
        intent.putExtra(Constants.STORY_COVER_POSITION, storyCoverPosition)
        intent.putExtra(Constants.STORY_POSITION, 0)
        intent.putExtra(Constants.INAPP_MESSAGE, inAppMessage)
        intent.putExtra(Constants.CONTENT_ID, contentId)
        intent.putExtra(Constants.PUBLIC_ID, publicId)
        context.startActivity(intent)
    }

    override fun getItemCount(): Int = storySet.covers.size

    fun setStoryList() {
        val storySet = inAppMessage.data.content.params.storySet
        val storySetId = storySet?.id

        if (storySet != null && storySetId != null) {
            val sortedStoryCovers = StoriesListView.inAppMessageCallback?.sortStoryCovers(storySet.covers, storySetId)
            if (sortedStoryCovers != null) {
                inAppMessage.data.content.params.storySet?.covers = sortedStoryCovers
            }
        }
    }

    inner class StoryHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        var tvStoryName: TextView = itemView.findViewById(R.id.tv_story_name)
        var civStory: CircularImageView = itemView.findViewById(R.id.civ_story)

        fun setCircleViewProperties(shown: Boolean) {

            civStory.visibility = View.VISIBLE

            civStory.apply {

                borderWidth = pxToDp(context, storySet.styling.headerCover.borderWidth.toFloat())

                if(shown){
                    borderColorStart = storySet.styling.headerCover.passiveColorInt
                    borderColorEnd = storySet.styling.headerCover.passiveColorInt
                } else {
                    if (storySet.styling.headerCover.fillerColorsInt.isEmpty()) {
                        borderColorStart = Color.WHITE
                        borderColorEnd = Color.WHITE
                    } else if (storySet.styling.headerCover.fillerColorsInt.size == 1) {
                        borderColorStart = storySet.styling.headerCover.fillerColorsInt[0]
                        borderColorEnd = storySet.styling.headerCover.fillerColorsInt[0]
                    } else {
                        borderColorStart = storySet.styling.headerCover.fillerColorsInt[0]
                        borderColorEnd = storySet.styling.headerCover.fillerColorsInt[1]
                    }
                }

                shadowEnable = false
                shadowRadius = 0f
                shadowColor = Color.RED
                shadowGravity = CircularImageView.ShadowGravity.CENTER
            }
        }
    }

    private fun cacheImagesBeforeDisplaying() {
        isFirstRun = false
        for (cover in storySet.covers) {
            if (cover.mediaUrl.isNotEmpty()) {
                try {
                    Glide.with(context)
                        .load(cover.mediaUrl)
                        .preload()
                } catch (e: Exception) {
                    Log.w("Story Activity", "URL for the image is empty!")
                }
            }
        }
    }

}