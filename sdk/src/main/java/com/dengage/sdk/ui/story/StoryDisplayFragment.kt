package com.dengage.sdk.ui.story

import android.annotation.SuppressLint
import android.content.Context
import android.graphics.Color
import android.graphics.drawable.GradientDrawable
import android.os.Bundle
import android.view.LayoutInflater
import android.view.MotionEvent
import android.view.View
import android.view.ViewGroup
import android.widget.Button
import android.widget.ProgressBar
import android.widget.TextView
import androidx.appcompat.widget.AppCompatImageView
import androidx.constraintlayout.widget.ConstraintLayout
import androidx.fragment.app.Fragment
import com.bumptech.glide.Glide
import com.dengage.sdk.R
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.model.MimeType
import com.dengage.sdk.domain.inappmessage.model.StoryCover
import com.dengage.sdk.domain.inappmessage.usecase.StoryEventType
import com.dengage.sdk.util.DengageLogger
import androidx.media3.common.MediaItem
import androidx.media3.exoplayer.ExoPlayer
import androidx.media3.common.PlaybackException
import androidx.media3.common.Player
import androidx.media3.ui.PlayerView
import java.util.ArrayList

class StoryDisplayFragment : Fragment(), StoriesProgressView.StoriesListener {

    private val storyPosition by lazy { arguments?.getInt(EXTRA_POSITION) ?: 0 }
    private val storyCover by lazy { arguments?.getSerializable(EXTRA_STORY_COVER) as StoryCover }
    private val stories by lazy { storyCover.stories.toCollection(ArrayList())  }
    private val inAppMessage by lazy { arguments?.getSerializable(EXTRA_INAPP_MESSAGE) as InAppMessage }

    private var simpleExoPlayer: ExoPlayer? = null
    private var pageViewOperator: StoryPageViewOperator? = null
    private var counter = 0
    private var pressTime = 0L
    private var limit = 500L
    private var onResumeCalled = false
    private var onVideoPrepared = false

    private lateinit var storyDisplayVideo: PlayerView
    private lateinit var storiesProgressView: StoriesProgressView
    private lateinit var storyDisplayImage: AppCompatImageView
    private lateinit var storyDisplayVideoProgress: ProgressBar
    private lateinit var storyOverlay: ConstraintLayout
    private lateinit var storyDisplayNick: TextView
    private lateinit var storyDisplayProfilePicture: AppCompatImageView
    private lateinit var next: View
    private lateinit var previous: View
    private lateinit var storyCloseButton: AppCompatImageView
    private lateinit var btnStory: Button

    override fun onCreateView(inflater: LayoutInflater, container: ViewGroup?, savedInstanceState: Bundle?): View? {
        return inflater.inflate(R.layout.fragment_story_display, container, false).apply {
            storyDisplayVideo = findViewById(R.id.storyDisplayVideo)
            storiesProgressView = findViewById(R.id.storiesProgressView)
            storyDisplayImage = findViewById(R.id.storyDisplayImage)
            storyDisplayVideoProgress = findViewById(R.id.storyDisplayVideoProgress)
            storyOverlay = findViewById(R.id.storyOverlay)
            storyDisplayNick = findViewById(R.id.storyDisplayNick)
            storyDisplayProfilePicture = findViewById(R.id.storyDisplayProfilePicture)
            next = findViewById(R.id.next)
            previous = findViewById(R.id.previous)
            storyCloseButton = findViewById(R.id.storyCloseButton)
            storyCloseButton.setOnClickListener {
                activity?.finish()
            }
            btnStory = findViewById(R.id.btn_story)
        }
    }

    override fun onViewCreated(view: View, savedInstanceState: Bundle?) {
        super.onViewCreated(view, savedInstanceState)
        storyDisplayVideo.useController = false
        updateStory()
        setUpUi()
    }

    override fun onAttach(context: Context) {
        super.onAttach(context)
        this.pageViewOperator = context as StoryPageViewOperator
    }

    override fun onStart() {
        super.onStart()
        counter = restorePosition()
    }

    override fun onPause() {
        super.onPause()
        simpleExoPlayer?.playWhenReady = false
        storiesProgressView.abandon()
    }

    override fun onComplete() {
        simpleExoPlayer?.release()
        pageViewOperator?.nextPageView()
    }

    override fun onStartProgress(currentIndex: Int) {
        val currentStory = stories[currentIndex]
        StoriesListView.inAppMessageCallback?.storyEvent(StoryEventType.STORY_DISPLAY, inAppMessage
                ,storyCover.id, storyCover.name, currentStory.id, currentStory.name)
        val storySetId = inAppMessage.data.content.params.storySet?.id
        if (storySetId != null) {
            StoriesListView.inAppMessageCallback?.setStoryCoverShown(storyCover.id, storySetId)
        }
        DengageLogger.verbose("StoryDisplayFragment onStartProgress: ${currentStory.name} : ${currentStory.id}")
    }

    override fun onPrev() {
        if (counter - 1 < 0) return
        --counter
        savePosition(counter)
        updateStory()
    }

    override fun onNext() {
        if (stories.size <= counter + 1) {
            return
        }
        ++counter
        savePosition(counter)
        updateStory()
    }

    override fun onDestroyView() {
        super.onDestroyView()
        simpleExoPlayer?.release()
    }

    private fun updateStory() {
        simpleExoPlayer?.stop()
        val currentStory = stories[counter]
        val isVideo = currentStory.kind == MimeType.VIDEO

        storyDisplayVideo.visibility = if (isVideo) View.VISIBLE else View.GONE
        storyDisplayImage.visibility = if (isVideo) View.GONE else View.VISIBLE
        storyDisplayVideoProgress.visibility = if (isVideo) View.VISIBLE else View.GONE

        if (isVideo) {
            setGradientBackground(null, storyDisplayVideo, counter)
            initializePlayer()
        } else {
            setGradientBackground(storyDisplayImage, null, counter)
            Glide.with(this).load(currentStory.mediaUrl).into(storyDisplayImage)
        }

        btnStory.visibility = if (currentStory.cta?.label.isNullOrEmpty()) View.GONE else View.VISIBLE
        currentStory.cta?.let {
            btnStory.apply {
                background = GradientDrawable().apply {
                    shape = GradientDrawable.RECTANGLE
                    cornerRadius = 10f * resources.displayMetrics.density
                    setColor(it.bgColorInt)
                }
                text = it.label
                setTextColor(it.textColorInt)
                btnStory.setOnClickListener {
                    StoriesListView.inAppMessageCallback?.storyEvent(StoryEventType.STORY_CLICK, inAppMessage
                            ,storyCover.id, storyCover.name, currentStory.id, currentStory.name, currentStory.cta.androidLink)
                    DengageLogger.verbose("StoryDisplayFragment STORY_CLICK: ${currentStory.name} : ${currentStory.id}")
                    activity?.finish()
                }
            }
        }
    }

    @SuppressLint("UnsafeOptInUsageError")
    private fun initializePlayer() {
        simpleExoPlayer?.release()
        simpleExoPlayer = ExoPlayer.Builder(requireContext()).build().apply {
            val mediaItem = stories[counter].mediaUrl?.let { MediaItem.fromUri(it) }
            mediaItem?.let { setMediaItem(it) }
            prepare()
            playWhenReady = onResumeCalled
        }

        storyDisplayVideo.apply {
            setShutterBackgroundColor(Color.BLACK)
            player = simpleExoPlayer
        }

        simpleExoPlayer?.addListener(playerListener)
    }

    private val playerListener = object : Player.Listener {
        override fun onPlayerError(error: PlaybackException) {
            storyDisplayVideoProgress.hide()
            if (counter == stories.size - 1) {
                pageViewOperator?.nextPageView()
            } else {
                storiesProgressView.skip()
            }
        }

        override fun onIsLoadingChanged(isLoading: Boolean) {
            if (isLoading) {
                storyDisplayVideoProgress.show()
                pressTime = System.currentTimeMillis()
                pauseCurrentStory()
            } else {
                storyDisplayVideoProgress.hide()
                storiesProgressView.getProgressWithIndex(counter).setDuration(simpleExoPlayer?.duration ?: 8000L)
                onVideoPrepared = true
                resumeCurrentStory()
            }
        }
    }

    private fun setGradientBackground(storyDisplayImage: AppCompatImageView? = null, storyDisplayVideo: PlayerView? = null, counter: Int) {
        var colors = intArrayOf(Color.WHITE, Color.WHITE)

        if (stories[counter].gradColors.size == 1) {
            colors = intArrayOf(
                stories[counter].gradColors[0],
                stories[counter].gradColors[0]
            )
        } else if  (stories[counter].gradColors.size > 1) {
            colors = stories[counter].gradColors.take(2).toIntArray()
        }

        val gradientDrawable = GradientDrawable(
            GradientDrawable.Orientation.TOP_BOTTOM, colors
        )
        storyDisplayImage?.background = gradientDrawable
        storyDisplayVideo?.background = gradientDrawable
    }


    private fun setUpUi() {
        val touchListener = object : StoryOnSwipeTouchListener(requireActivity()) {
            override fun onSwipeTop() {
                //Toast.makeText(activity, "onSwipeTop", Toast.LENGTH_LONG).show()
            }

            override fun onSwipeBottom() {
                //TODO: check
                activity?.finish()
                //Toast.makeText(activity, "onSwipeBottom", Toast.LENGTH_LONG).show()
            }

            override fun onClick(view: View) {
                when (view) {
                    next -> {
                        if (counter == stories.size - 1) {
                            pageViewOperator?.nextPageView()
                        } else {
                            storiesProgressView.skip()
                        }
                    }
                    previous -> {
                        if (counter == 0) {
                            pageViewOperator?.backPageView()
                        } else {
                            storiesProgressView.reverse()
                        }
                    }
                }
            }

            override fun onLongClick() {
                hideStoryOverlay()
            }

            override fun onTouchView(view: View, event: MotionEvent): Boolean {
                super.onTouchView(view, event)
                when (event.action) {
                    MotionEvent.ACTION_DOWN -> {
                        pressTime = System.currentTimeMillis()
                        pauseCurrentStory()
                        return false
                    }
                    MotionEvent.ACTION_UP -> {
                        showStoryOverlay()
                        resumeCurrentStory()
                        return limit < System.currentTimeMillis() - pressTime
                    }
                }
                return false
            }
        }
        previous.setOnTouchListener(touchListener)
        next.setOnTouchListener(touchListener)

        storiesProgressView.setStoriesCountDebug(
            stories.size, position = arguments?.getInt(EXTRA_POSITION) ?: -1
        )
        storiesProgressView.setAllStoryDuration(STORY_DURATION)
        storiesProgressView.setStoriesListener(this)

        Glide.with(this).load(storyCover.mediaUrl).circleCrop().into(storyDisplayProfilePicture)
        storyDisplayNick.text = storyCover.name
    }

    private fun showStoryOverlay() {
        if (storyOverlay.alpha != 0F) return

        storyOverlay.animate()
            .setDuration(100)
            .alpha(1F)
            .start()
    }

    private fun hideStoryOverlay() {
        if (storyOverlay.alpha != 1F) return

        storyOverlay.animate()
            .setDuration(200)
            .alpha(0F)
            .start()
    }

    override fun onResume() {
        super.onResume()
        onResumeCalled = true

        if (stories[counter].kind == MimeType.VIDEO && !onVideoPrepared) {
            simpleExoPlayer?.playWhenReady = false
            return
        }

        simpleExoPlayer?.seekTo(5)
        simpleExoPlayer?.playWhenReady = true
        if (counter == 0) {
            storiesProgressView.startStories()
        } else {
            counter = StoryActivity.progressState.get(arguments?.getInt(EXTRA_POSITION) ?: 0)
            storiesProgressView.startStories(counter)
        }
    }

    private fun savePosition(pos: Int) {
        StoryActivity.progressState.put(storyPosition, pos)
    }

    private fun restorePosition(): Int {
        return StoryActivity.progressState.get(storyPosition)
    }

    fun pauseCurrentStory() {
        simpleExoPlayer?.playWhenReady = false
        storiesProgressView.pause()
    }

    fun resumeCurrentStory() {
        if (onResumeCalled) {
            simpleExoPlayer?.playWhenReady = true
            showStoryOverlay()
            storiesProgressView.resume()
        }
    }

    companion object {
        private const val EXTRA_POSITION = "EXTRA_POSITION"
        private const val EXTRA_STORY_COVER = "EXTRA_STORY_COVER"
        private const val EXTRA_INAPP_MESSAGE = "EXTRA_INAPP_MESSAGE"
        private const val STORY_DURATION = 10000L
        fun newInstance(storyPosition: Int, storyCover: StoryCover, inAppMessage: InAppMessage): StoryDisplayFragment {
            return StoryDisplayFragment().apply {
                arguments = Bundle().apply {
                    putInt(EXTRA_POSITION, storyPosition)
                    putSerializable(EXTRA_STORY_COVER, storyCover)
                    putSerializable(EXTRA_INAPP_MESSAGE, inAppMessage)
                }
            }
        }
    }
}