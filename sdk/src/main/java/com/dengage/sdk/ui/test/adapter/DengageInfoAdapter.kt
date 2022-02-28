package com.dengage.sdk.ui.test.adapter

import android.content.ClipData
import android.content.ClipboardManager
import android.content.Context.CLIPBOARD_SERVICE
import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import android.widget.Toast
import androidx.recyclerview.widget.RecyclerView
import com.dengage.sdk.R


class DengageInfoAdapter(
    private val infoPairs: MutableList<Pair<String, String?>>
) : RecyclerView.Adapter<DengageInfoAdapter.ViewHolder>() {

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): ViewHolder {
        return ViewHolder(LayoutInflater.from(parent.context).inflate(R.layout.recycler_item_dengage_info, parent, false))
    }

    override fun onBindViewHolder(holder: ViewHolder, position: Int) {
        holder.textViewTitle.text = infoPairs[position].first
        holder.textViewMessage.text = infoPairs[position].second

        holder.textViewMessage.setOnClickListener {
            val clipboard = holder.textViewMessage.context.getSystemService(CLIPBOARD_SERVICE) as ClipboardManager
            val clip = ClipData.newPlainText("Copied", infoPairs[holder.bindingAdapterPosition].second)
            clipboard.setPrimaryClip(clip)
            Toast.makeText(holder.textViewMessage.context, "Copied to clipboard", Toast.LENGTH_LONG).show()
        }
    }

    override fun getItemCount(): Int {
        return infoPairs.size
    }

    fun updateItems(infoPairs: MutableList<Pair<String, String>>) {
        this.infoPairs.clear()
        this.infoPairs.addAll(infoPairs)
        notifyDataSetChanged()
    }

    inner class ViewHolder(view: View) : RecyclerView.ViewHolder(view) {
        var textViewTitle: TextView = view.findViewById(R.id.textViewTitle) as TextView
        var textViewMessage: TextView = view.findViewById(R.id.textViewMessage) as TextView
    }
}