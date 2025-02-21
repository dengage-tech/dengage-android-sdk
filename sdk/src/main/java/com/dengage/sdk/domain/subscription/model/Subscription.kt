package com.dengage.sdk.domain.subscription.model

import com.google.gson.annotations.SerializedName
import java.io.Serializable

data class Subscription(
    @SerializedName("integrationKey") var integrationKey: String = "",
    @SerializedName("token") var token: String? = null,
    @SerializedName("appVersion") var appVersion: String? = "",
    @SerializedName("sdkVersion") var sdkVersion: String = "",
    @SerializedName("udid") var deviceId: String? = "",
    @SerializedName("advertisingId") var advertisingId: String = "",
    @SerializedName("carrierId") var carrierId: String = "",
    @SerializedName("contactKey") var contactKey: String? = "",
    @SerializedName("permission") var permission: Boolean? = true,
    @SerializedName("trackingPermission") var trackingPermission: Boolean = true,
    @SerializedName("tokenType") var tokenType: String = "A",
    @SerializedName("webSubscription") var webSubscription: String? = null,
    @SerializedName("testGroup") var testGroup: String = "",
    @SerializedName("country") var country: String? = "",
    @SerializedName("language") var language: String = "",
    @SerializedName("timezone") var timezone: String = "",
    @SerializedName("partner_device_id") var partnerDeviceId: String? = "",
    @SerializedName("locationPermission") var locationPermission: String? = ""

) : Serializable {

    fun getSafeDeviceId(): String {
        return deviceId ?: ""
    }

    fun getContactKeyForVisitorInfoParameter() :String?
    {
        if(contactKey.isNullOrEmpty()) return null else return contactKey
    }
}
