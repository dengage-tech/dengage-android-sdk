package com.dengage.android.kotlin.sample.ui.base

import android.view.View
import android.view.ViewGroup
import androidx.annotation.LayoutRes
import androidx.recyclerview.widget.RecyclerView
import com.dengage.android.kotlin.sample.extensions.inflate

abstract class BaseRecyclerViewHolder<T> : RecyclerView.ViewHolder,
    View.OnClickListener {

    private var onRecyclerItemClickListener: OnRecyclerItemClickListener? = null

    /**
     * Constructor.
     *
     * @param parentView parent
     * @param layoutId   layout resource to inflate
     */
    constructor(parentView: ViewGroup, @LayoutRes layoutId: Int)
            : this(parentView.inflate(layoutId))

    /**
     * Constructor.
     *
     * @param itemView item view
     */
    constructor(itemView: View) : super(itemView)

    /**
     * Calls to update the contents with the item.
     *
     *
     * Override to set up some private fields to be used by RecyclerView.
     */
    abstract fun bindItem(item: T)

    /**
     * Register a callback to be invoked when this item is clicked.
     *
     * @param onItemClickListener callback
     */
    fun setItemClickListener(onRecyclerItemClickListener: OnRecyclerItemClickListener?) {
        onRecyclerItemClickListener?.let {
            this.onRecyclerItemClickListener = onRecyclerItemClickListener
            this.itemView.setOnClickListener(this)
        }
    }

    override fun onClick(itemView: View) {
        onRecyclerItemClickListener?.onItemClick(adapterPosition)
    }
}

@Suppress("UNCHECKED_CAST")
fun BaseRecyclerViewHolder<*>.any(): BaseRecyclerViewHolder<Any> =
    this as BaseRecyclerViewHolder<Any>