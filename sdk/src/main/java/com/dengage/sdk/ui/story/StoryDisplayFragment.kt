package com.dengage.sdk.ui.story

import android.annotation.SuppressLint
import android.content.Context
import android.content.res.Configuration
import android.graphics.Color
import android.graphics.Typeface
import android.graphics.drawable.GradientDrawable
import android.os.Build
import android.os.Bundle
import android.util.TypedValue
import android.view.Gravity
import android.view.LayoutInflater
import android.view.MotionEvent
import android.view.View
import android.view.ViewGroup
import android.widget.Button
import android.widget.ImageView
import android.widget.ProgressBar
import android.widget.TextView
import androidx.annotation.OptIn
import androidx.appcompat.widget.AppCompatImageView
import androidx.constraintlayout.widget.ConstraintLayout
import androidx.fragment.app.Fragment
import com.bumptech.glide.Glide
import com.dengage.sdk.R
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.model.MimeType
import com.dengage.sdk.domain.inappmessage.model.Story
import com.dengage.sdk.domain.inappmessage.model.StoryCover
import com.dengage.sdk.domain.inappmessage.model.StorySet
import com.dengage.sdk.domain.inappmessage.model.StorySetButton
import com.dengage.sdk.domain.inappmessage.model.StorySetButtonTitle
import com.dengage.sdk.domain.inappmessage.model.StorySetFontWeight
import com.dengage.sdk.domain.inappmessage.model.StorySetImagePositioning
import com.dengage.sdk.domain.inappmessage.model.StorySetStyling
import com.dengage.sdk.domain.inappmessage.model.parseColor
import com.dengage.sdk.domain.inappmessage.usecase.StoryEventType
import com.dengage.sdk.util.DengageLogger
import com.dengage.sdk.util.EdgeToEdgeUtils
import androidx.media3.common.MediaItem
import androidx.media3.exoplayer.ExoPlayer
import androidx.media3.common.PlaybackException
import androidx.media3.common.Player
import androidx.media3.common.util.UnstableApi
import androidx.media3.ui.AspectRatioFrameLayout
import androidx.media3.ui.PlayerView
import java.util.ArrayList

class StoryDisplayFragment : Fragment(), StoriesProgressView.StoriesListener {

    private val storyPosition by lazy { arguments?.getInt(EXTRA_POSITION) ?: 0 }
    private val storyCover by lazy { 
        arguments?.getSerializable(EXTRA_STORY_COVER) as? StoryCover 
            ?: throw IllegalStateException("StoryCover is null or invalid")
    }
    private val stories by lazy { storyCover.stories.toCollection(ArrayList())  }
    private val inAppMessage by lazy { 
        arguments?.getSerializable(EXTRA_INAPP_MESSAGE) as? InAppMessage 
            ?: throw IllegalStateException("InAppMessage is null or invalid")
    }

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
        EdgeToEdgeUtils.setupEdgeToEdgeInsets(view)
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
        // Persist immediately so onResume's progressState lookup doesn't reset counter to 0.
        savePosition(counter)
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
        if (currentIndex >= stories.size) {
            DengageLogger.error("StoryDisplayFragment onStartProgress: Index $currentIndex is out of bounds for stories size ${stories.size}")
            return
        }
        val currentStory = stories[currentIndex]
        StoriesListView.inAppMessageCallback?.storyEvent(StoryEventType.STORY_DISPLAY, inAppMessage
                ,storyCover.id, storyCover.name, currentStory.id, currentStory.name)
        val storySetId = inAppMessage.data.content.params.storySet?.id
        if (storySetId != null) {
            // Record this individual story as viewed. The cover escalates to "shown"
            // when every story id under it has been recorded.
            StoriesListView.inAppMessageCallback?.setStoryViewed(
                storyId = currentStory.id,
                storyCoverId = storyCover.id,
                storySetId = storySetId,
                allStoryIdsInCover = stories.map { it.id }
            )
        }
        // Always remember the last index the user was on so the cover resumes at lastIndex + 1.
        StoriesListView.inAppMessageCallback?.setLastViewedStoryIndex(storyCover.id, currentIndex)
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
        if (counter >= stories.size) {
            DengageLogger.error("StoryDisplayFragment updateStory: Counter $counter is out of bounds for stories size ${stories.size}")
            return
        }
        val currentStory = stories[counter]
        val isVideo = currentStory.kind == MimeType.VIDEO

        storyDisplayVideo.visibility = if (isVideo) View.VISIBLE else View.GONE
        storyDisplayImage.visibility = if (isVideo) View.GONE else View.VISIBLE
        storyDisplayVideoProgress.visibility = if (isVideo) View.VISIBLE else View.GONE

        applyMediaPositioning(currentStory)

        if (isVideo) {
            setStoryBackground(null, storyDisplayVideo, currentStory)
            initializePlayer()
        } else {
            setStoryBackground(storyDisplayImage, null, currentStory)
            currentStory.mediaUrl?.let { mediaUrl ->
                Glide.with(this).load(mediaUrl).into(storyDisplayImage)
            }
        }

        applyCtaButton(currentStory)
    }

    @OptIn(UnstableApi::class)
    private fun applyMediaPositioning(currentStory: Story) {
        val positioning = currentStory.imagePositioningEnum
        storyDisplayImage.scaleType = when (positioning) {
            StorySetImagePositioning.FILL -> ImageView.ScaleType.CENTER_CROP
            StorySetImagePositioning.FIT -> ImageView.ScaleType.FIT_CENTER
            null -> ImageView.ScaleType.FIT_CENTER
        }
        storyDisplayVideo.resizeMode = when (positioning) {
            StorySetImagePositioning.FILL -> AspectRatioFrameLayout.RESIZE_MODE_ZOOM
            else -> AspectRatioFrameLayout.RESIZE_MODE_FIT
        }
    }

    private fun applyCtaButton(currentStory: Story) {
        val cta = currentStory.cta
        val visible = cta?.isEnabled == true && cta.label.isNotEmpty()
        btnStory.visibility = if (visible) View.VISIBLE else View.GONE
        if (!visible || cta == null) return

        val styling = storySet.styling
        val dark = isDarkMode(styling)
        val titleStyle = styling.resolvedButtonTitle(dark)
        val boxStyle = styling.resolvedButton(dark)
        val density = resources.displayMetrics.density

        val bgColor = styling.resolvedButtonBackgroundColor(cta, dark)?.let { parseColor(it) }
            ?: cta.bgColorInt
        val borderColor = styling.resolvedButtonBorderColor(dark)?.let { parseColor(it) }
        val textColor = styling.resolvedButtonTextColor(cta, dark)?.let { parseColor(it) }
            ?: cta.textColorInt
        val cornerRadiusPx = (boxStyle?.borderRadius?.toFloat() ?: 10f) * density

        val background = GradientDrawable().apply {
            shape = GradientDrawable.RECTANGLE
            cornerRadius = cornerRadiusPx
            setColor(bgColor)
            if (borderColor != null) {
                setStroke((1 * density).toInt().coerceAtLeast(1), borderColor)
            }
        }

        btnStory.apply {
            this.background = background
            text = cta.label
            setTextColor(textColor)

            titleStyle?.fontSize?.let {
                setTextSize(TypedValue.COMPLEX_UNIT_SP, it.toFloat())
            }

            val typefaceStyle = if (titleStyle?.fontWeight == StorySetFontWeight.BOLD) Typeface.BOLD else Typeface.NORMAL
            val familyName = titleStyle?.fontFamily?.takeIf { it.isNotBlank() }
                ?: styling.effectiveFontFamily
            typeface = StoryFontResolver.resolve(requireContext(), familyName, typefaceStyle)

            gravity = when (titleStyle?.textAlign?.lowercase()) {
                "right" -> Gravity.END or Gravity.CENTER_VERTICAL
                "left" -> Gravity.START or Gravity.CENTER_VERTICAL
                else -> Gravity.CENTER
            }

            val padding = boxStyle?.padding
            if (padding != null && boxStyle.fitContent != true) {
                setPadding(
                    (padding.left * density).toInt(),
                    (padding.top * density).toInt(),
                    (padding.right * density).toInt(),
                    (padding.bottom * density).toInt()
                )
            } else if (boxStyle?.fitContent == true) {
                setPadding(0, 0, 0, 0)
            }

            val lp = layoutParams
            // height is treated as minimum so that padding can still grow the button.
            val minH = boxStyle?.height?.let { (it * density).toInt() } ?: 0
            minimumHeight = minH
            minHeight = minH
            lp.height = ViewGroup.LayoutParams.WRAP_CONTENT
            lp.width = if (boxStyle?.fitContent == true) ViewGroup.LayoutParams.WRAP_CONTENT
            else ViewGroup.LayoutParams.MATCH_PARENT
            layoutParams = lp

            setOnClickListener {
                StoriesListView.inAppMessageCallback?.storyEvent(StoryEventType.STORY_CLICK, inAppMessage,
                    storyCover.id, storyCover.name, currentStory.id, currentStory.name, cta.androidLink)
                DengageLogger.verbose("StoryDisplayFragment STORY_CLICK: ${currentStory.name} : ${currentStory.id}")
                activity?.finish()
            }
        }
    }

    @SuppressLint("UnsafeOptInUsageError")
    private fun initializePlayer() {
        simpleExoPlayer?.release()
        if (counter >= stories.size) {
            DengageLogger.error("StoryDisplayFragment initializePlayer: Counter $counter is out of bounds for stories size ${stories.size}")
            return
        }
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
            if (stories.isEmpty() || counter == stories.size - 1) {
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

    private fun setStoryBackground(
        storyDisplayImage: AppCompatImageView? = null,
        storyDisplayVideo: PlayerView? = null,
        currentStory: Story
    ) {
        val styling = storySet.styling
        val dark = isDarkMode(styling)

        // Per migration guide fallback chain:
        // dark.storyBackgroundColor -> styling.storyBackgroundColor -> story.bgColors[0].
        val resolved = styling.resolvedStoryBackgroundColor(currentStory, dark)?.let { parseColor(it) }
        val background = if (resolved != null) {
            GradientDrawable().apply { setColor(resolved) }
        } else {
            val grad = currentStory.gradColors
            val colors = when (grad.size) {
                0 -> intArrayOf(Color.WHITE, Color.WHITE)
                1 -> intArrayOf(grad[0], grad[0])
                else -> grad.take(2).toIntArray()
            }
            GradientDrawable(GradientDrawable.Orientation.TOP_BOTTOM, colors)
        }
        storyDisplayImage?.background = background
        storyDisplayVideo?.background = background
    }

    private fun isDarkMode(styling: StorySetStyling): Boolean {
        // Per migration guide: presence of styling.dark signals dark mode is configured.
        if (styling.dark == null) return false
        val nightFlags = resources.configuration.uiMode and Configuration.UI_MODE_NIGHT_MASK
        return nightFlags == Configuration.UI_MODE_NIGHT_YES
    }

    private val storySet: StorySet
        get() = inAppMessage.data.content.params.storySet ?: StorySet()


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
                        if (stories.isEmpty() || counter == stories.size - 1) {
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
        // Per-story duration overrides the default; story.duration is in seconds.
        stories.forEachIndexed { index, story ->
            val durationMs = story.duration?.takeIf { it > 0 }?.times(1000L)
            if (durationMs != null) {
                storiesProgressView.getProgressWithIndex(index).setDuration(durationMs)
            }
        }
        storiesProgressView.setStoriesListener(this)

        storyCover.mediaUrl?.let { mediaUrl ->
            Glide.with(this).load(mediaUrl).circleCrop().into(storyDisplayProfilePicture)
        }
        storyDisplayNick.text = storyCover.name ?: ""
        storySet.styling.effectiveFontFamily?.let { family ->
            val weight = if (storySet.styling.headerTitle.fontWeight == StorySetFontWeight.BOLD) Typeface.BOLD else Typeface.NORMAL
            storyDisplayNick.typeface = StoryFontResolver.resolve(requireContext(), family, weight)
        }
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

        if (counter >= stories.size) {
            DengageLogger.error("StoryDisplayFragment onResume: Counter $counter is out of bounds for stories size ${stories.size}")
            return
        }

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
        // Within-session state wins: if we already have a position for this cover
        // from a previous fragment lifecycle in the same activity, use it.
        val stored = StoryActivity.progressState.get(storyPosition, -1)
        if (stored >= 0) return stored
        // Fresh open: resume at lastViewedIndex + 1. If past the end, replay from 0.
        val lastIdx = StoriesListView.inAppMessageCallback?.getLastViewedStoryIndex(storyCover.id) ?: -1
        if (lastIdx < 0) return 0
        val nextIdx = lastIdx + 1
        return if (nextIdx >= stories.size) 0 else nextIdx
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