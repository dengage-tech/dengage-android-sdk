package com.dengage.android.kotlin.sample.ui.base

import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView

abstract class RecyclerViewPaginationListener(
    private val linearLayoutManager: LinearLayoutManager,
    private val loadingItemCount: Int,
    private val loadingThreshold: Int = 15
) : RecyclerView.OnScrollListener() {

    private var currentPage = 0

    override fun onScrolled(recyclerView: RecyclerView, dx: Int, dy: Int) {
        // scrolling up
        if (dy < 0) return

        val visibleItemCount = recyclerView.childCount
        val totalItemCount = linearLayoutManager.itemCount
        val firstVisibleItemPosition = linearLayoutManager.findFirstVisibleItemPosition()

        synchronized(this) {
            if (!isLoading()
                && totalItemCount % loadingItemCount == 0
                && totalItemCount - visibleItemCount <=
                firstVisibleItemPosition + loadingThreshold
            ) {
                currentPage++
                loadMore(currentPage)
            }
        }
    }

    abstract fun loadMore(currentPage: Int)

    abstract fun isLoading(): Boolean

    /*(var layoutManager: LinearLayoutManager) : RecyclerView.OnScrollListener() {

        abstract fun isLastPage(): Boolean

        abstract fun isLoading(): Boolean

        override fun onScrolled(recyclerView: RecyclerView?, dx: Int, dy: Int) {
            super.onScrolled(recyclerView, dx, dy)

            val visibleItemCount = layoutManager.childCount
            val totalItemCount = layoutManager.itemCount
            val firstVisibleItemPosition = layoutManager.findFirstVisibleItemPosition()

            if (!isLoading() && !isLastPage()) {
                if (visibleItemCount + firstVisibleItemPosition >= totalItemCount && firstVisibleItemPosition >= 0) {
                    loadMoreItems()
                }//                    && totalItemCount >= ClothesFragment.itemsCount
            }
        }
        abstract fun loadMoreItems()
    }*/

}