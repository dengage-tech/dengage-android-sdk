package com.dengage.sdk.ui.story

import android.animation.Animator
import android.animation.ValueAnimator
import android.annotation.SuppressLint
import android.os.Bundle
import android.util.SparseIntArray
import androidx.appcompat.app.AppCompatActivity
import androidx.interpolator.view.animation.FastOutSlowInInterpolator
import com.bumptech.glide.Glide
import com.dengage.sdk.R
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.model.MimeType
import com.dengage.sdk.domain.inappmessage.model.StoryCover
import com.dengage.sdk.util.Constants
import com.dengage.sdk.util.DengageLogger

interface StoryPageViewOperator {
    fun backPageView()
    fun nextPageView()
}

class StoryActivity : AppCompatActivity(),
    StoryPageViewOperator {

    private lateinit var pagerAdapter: StoryPagerAdapter
    private lateinit var viewPager: FixedViewPager

    private var inAppMessage: InAppMessage? = null
    private var storyCoverPosition: Int = 0
    private var storyPosition: Int = 0
    private var contentId: String? = null
    private var publicId: String? = null

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_story)
        viewPager = findViewById(R.id.viewPager)
        storyCoverPosition = intent.getIntExtra(Constants.STORY_COVER_POSITION, 0)
        storyPosition = intent.getIntExtra(Constants.STORY_POSITION, 0)
        inAppMessage = intent.getSerializableExtra(Constants.INAPP_MESSAGE) as InAppMessage?
        contentId = intent.getStringExtra(Constants.CONTENT_ID)
        publicId = intent.getStringExtra(Constants.PUBLIC_ID)
        setUpPager()
    }


    override fun backPageView() {
        if (viewPager.currentItem > 0) {
            try {
                fakeDrag(false)
            } catch (e: Exception) {
                DengageLogger.error(e.message)
            }
        }
    }

    override fun nextPageView() {
        if (viewPager.currentItem + 1 < (viewPager.adapter?.count ?: 0)) {
            try {
                fakeDrag(true)
            } catch (e: Exception) {
                DengageLogger.error(e.message)
            }
        } else {
            //there is no next story
            //Toast.makeText(this, "All stories displayed.", Toast.LENGTH_LONG).show()
        }
    }

    private fun setUpPager() {
        val storyCovers = inAppMessage?.data?.content?.params?.storySet?.covers ?: emptyList()
        preLoadStories(storyCovers)

        pagerAdapter = StoryPagerAdapter(supportFragmentManager, inAppMessage!!)
        viewPager.adapter = pagerAdapter
        viewPager.currentItem = storyCoverPosition
        viewPager.setPageTransformer(
            true,
            StoryCubeOutTransformer()
        )
        viewPager.addOnPageChangeListener(object : StoryPageChangeListener() {
            override fun onPageSelected(position: Int) {
                super.onPageSelected(position)
                storyCoverPosition = position
            }

            override fun onPageScrollCanceled() {
                currentFragment().resumeCurrentStory()
            }
        })
    }

    private fun preLoadStories(storyCovers: List<StoryCover>) {
        val imageList = mutableListOf<String>()
        val videoList = mutableListOf<String>()

        storyCovers.forEach { storyCover ->
            storyCover.stories.forEach { story ->
                if (story.kind == MimeType.VIDEO) {
                    videoList.add(story.mediaUrl ?: "")
                } else {
                    imageList.add(story.mediaUrl ?: "")
                }
            }
        }
        preLoadImages(imageList)
    }

    private fun preLoadImages(imageList: MutableList<String>) {
        imageList.forEach { imageStory ->
            Glide.with(this).load(imageStory).preload()
        }
    }

    private fun currentFragment(): StoryDisplayFragment {
        return pagerAdapter.findFragmentByPosition(viewPager, storyCoverPosition) as StoryDisplayFragment
    }

    private var prevDragPosition = 0

    private fun fakeDrag(forward: Boolean) {
        if (prevDragPosition == 0 && viewPager.beginFakeDrag()) {
            ValueAnimator.ofInt(0, viewPager.width).apply {
                duration = 400L
                interpolator = FastOutSlowInInterpolator()
                addListener(object : Animator.AnimatorListener {
                    override fun onAnimationRepeat(p0: Animator) {}

                    override fun onAnimationEnd(animation: Animator) {
                        removeAllUpdateListeners()
                        if (viewPager.isFakeDragging) {
                            viewPager.endFakeDrag()
                        }
                        prevDragPosition = 0
                    }

                    override fun onAnimationCancel(animation: Animator) {
                        removeAllUpdateListeners()
                        if (viewPager.isFakeDragging) {
                            viewPager.endFakeDrag()
                        }
                        prevDragPosition = 0
                    }

                    override fun onAnimationStart(p0: Animator) {}
                })
                addUpdateListener {
                    if (!viewPager.isFakeDragging) return@addUpdateListener
                    val dragPosition: Int = it.animatedValue as Int
                    val dragOffset: Float =
                        ((dragPosition - prevDragPosition) * if (forward) -1 else 1).toFloat()
                    prevDragPosition = dragPosition
                    viewPager.fakeDragBy(dragOffset)
                }
            }.start()
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    override fun onDestroy() {
        progressState.clear()
        storiesListAdapter?.setStoryList()
        storiesListAdapter?.notifyDataSetChanged()
        super.onDestroy()
    }

    companion object {
        val progressState = SparseIntArray()
        @SuppressLint("StaticFieldLeak")
        var storiesListAdapter: StoriesListAdapter? = null

    }
}
