package com.dengage.android.kotlin.sample.ui.adapter

import android.text.Editable
import android.text.TextWatcher
import android.view.LayoutInflater
import android.view.ViewGroup
import androidx.recyclerview.widget.RecyclerView
import com.dengage.android.kotlin.sample.databinding.ItemDeviceInfoBinding

class DeviceInfoAdapter : RecyclerView.Adapter<DeviceInfoAdapter.DeviceInfoViewHolder>() {

    private val deviceInfoList = mutableListOf<Pair<String, String>>()

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): DeviceInfoViewHolder {
        val binding = ItemDeviceInfoBinding.inflate(
            LayoutInflater.from(parent.context), parent, false
        )
        return DeviceInfoViewHolder(binding)
    }

    override fun onBindViewHolder(holder: DeviceInfoViewHolder, position: Int) {
        holder.bind(deviceInfoList[position])
    }

    override fun getItemCount(): Int = deviceInfoList.size

    fun addDeviceInfo(key: String, value: String) {
        deviceInfoList.add(Pair(key, value))
        notifyItemInserted(deviceInfoList.size - 1)
    }

    fun clearData() {
        deviceInfoList.clear()
        notifyDataSetChanged()
    }

    fun getDeviceInfoList(): List<Pair<String, String>> = deviceInfoList

    inner class DeviceInfoViewHolder(private val binding: ItemDeviceInfoBinding) :
        RecyclerView.ViewHolder(binding.root) {

        fun bind(deviceInfo: Pair<String, String>) {
            binding.etKey.setText(deviceInfo.first)
            binding.etValue.setText(deviceInfo.second)

            binding.btnRemove.setOnClickListener {
                val position = adapterPosition
                if (position != RecyclerView.NO_POSITION) {
                    deviceInfoList.removeAt(position)
                    notifyItemRemoved(position)
                }
            }

            binding.etKey.addTextChangedListener(object : TextWatcher {
                override fun afterTextChanged(s: Editable?) {
                    if (adapterPosition != RecyclerView.NO_POSITION) {
                        deviceInfoList[adapterPosition] =
                            deviceInfoList[adapterPosition].copy(first = s.toString())
                    }
                }

                override fun beforeTextChanged(s: CharSequence?, start: Int, count: Int, after: Int) {}
                override fun onTextChanged(s: CharSequence?, start: Int, before: Int, count: Int) {}
            })

            binding.etValue.addTextChangedListener(object : TextWatcher {
                override fun afterTextChanged(s: Editable?) {
                    if (adapterPosition != RecyclerView.NO_POSITION) {
                        deviceInfoList[adapterPosition] =
                            deviceInfoList[adapterPosition].copy(second = s.toString())
                    }
                }

                override fun beforeTextChanged(s: CharSequence?, start: Int, count: Int, after: Int) {}
                override fun onTextChanged(s: CharSequence?, start: Int, before: Int, count: Int) {}
            })
        }
    }
}
