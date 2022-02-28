package com.dengage.android.kotlin.sample.ui.base

import android.view.ViewGroup
import androidx.recyclerview.widget.RecyclerView

abstract class BaseRecyclerViewAdapter(items: List<Any>? = null) :
    RecyclerView.Adapter<BaseRecyclerViewHolder<*>>() {

    protected val itemList = ArrayList<Any>()

    private var recyclerItemClickListener: OnRecyclerItemClickListener? = null

    init {
        items?.let {
            this.itemList.addAll(it)
        }
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): BaseRecyclerViewHolder<*> {
        val viewHolder = createNewViewHolder(parent, viewType)

        viewHolder.setItemClickListener(recyclerItemClickListener)

        return viewHolder
    }

    override fun onBindViewHolder(viewHolder: BaseRecyclerViewHolder<*>, position: Int) {
        getItem(position)?.let { item ->
            viewHolder.any().bindItem(item)
        }
    }

    override fun getItemCount(): Int = itemList.size

    /**
     * Returns the itemList in the data set held by this adapter.
     *
     * @return The itemList in this adapter.
     */
    fun getItems(): ArrayList<Any> {
        return ArrayList(itemList)
    }

    /**
     * Sets new itemList to display in this adapter.
     *
     * @param items The itemList to display in this adapter.
     */
    fun setItems(items: List<Any>?) {
        this.itemList.clear()
        this.itemList.addAll(items ?: arrayListOf())
        notifyDataSetChanged()
    }

    /**
     * Returns the item at the specified position in this adapter.
     *
     * @param position position of the item to return
     * @return the item at the specified position in this adapter
     */
    fun getItem(position: Int): Any? {
        return itemList.getOrNull(position)
    }

    /**
     * Add new itemList to display in this adapter.
     *
     * @param items The itemList to display in this adapter.
     */
    fun addItems(items: List<Any>) {
        val previousSize = itemCount
        this.itemList.addAll(items)
        notifyItemRangeInserted(previousSize, items.size)
    }

    /**
     * Add a new item to display in this adapter.
     *
     * @param item The item to add.
     */
    fun addItem(item: Any, position: Int) {
        this.itemList.add(position, item)
        notifyItemInserted(position)
    }

    /**
     * Update an item to display in this adapter.
     *
     * @param item The item to add.
     */
    fun updateItem(item: Any, position: Int) {
        this.itemList[position] = item
        notifyItemChanged(position)
    }

    /**
     * Remove the item at position from this adapter.
     *
     * @param position The position at which the item to remove.
     */
    fun removeItem(position: Int) {
        this.itemList.removeAt(position)
        notifyItemRemoved(position)
    }

    /**
     * Clear all itemList in this adapter.
     */
    fun clearItems() {
        this.itemList.clear()
        notifyDataSetChanged()
    }

    /**
     * Register a callback to be invoked when an item is clicked.
     *
     * @param recyclerItemClickListener callback
     */
    fun setItemClickListener(recyclerItemClickListener: OnRecyclerItemClickListener?) {
        this.recyclerItemClickListener = recyclerItemClickListener
    }


    /**
     * Called when a new ViewHolder of the given type is needed to represent the RecyclerView item.
     *
     * @param parent   parent layout
     * @param viewType the integer value identifying the type of the view
     * @return a new viewHolder instance
     */
    protected abstract fun createNewViewHolder(
        parent: ViewGroup,
        viewType: Int
    ): BaseRecyclerViewHolder<*>
}