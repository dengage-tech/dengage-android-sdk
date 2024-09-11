package com.dengage.sdk.ui.story

import androidx.fragment.app.Fragment
import androidx.fragment.app.FragmentManager
import androidx.fragment.app.FragmentStatePagerAdapter
import androidx.viewpager.widget.ViewPager
import com.dengage.sdk.domain.inappmessage.model.InAppMessage
import com.dengage.sdk.domain.inappmessage.model.StorySet

class StoryPagerAdapter(
    fragmentManager: FragmentManager,
    var inAppMessage: InAppMessage
) : FragmentStatePagerAdapter(fragmentManager, BEHAVIOR_RESUME_ONLY_CURRENT_FRAGMENT) {

    private val storySet: StorySet
        get() = inAppMessage.data.content.params.storySet ?: StorySet()

    override fun getItem(position: Int): Fragment =
        StoryDisplayFragment.newInstance(position, storySet.covers[position], inAppMessage)

    override fun getCount(): Int {
        return storySet.covers.size
    }

    fun findFragmentByPosition(viewPager: ViewPager, position: Int): Fragment? {
        try {
            val f = instantiateItem(viewPager, position)
            return f as? Fragment
        } finally {
            finishUpdate(viewPager)
        }
    }
}