package com.dengage.android.kotlin.sample.network

import com.google.gson.JsonObject
import okhttp3.ResponseBody
import org.json.JSONObject
import retrofit2.Call
import retrofit2.http.Body
import retrofit2.http.DELETE
import retrofit2.http.GET
import retrofit2.http.Header
import retrofit2.http.POST
import retrofit2.http.Path
import retrofit2.http.Query


interface Api {
    @GET("/rest/V1/carts/mine")
    fun generateCartIdWithCart(@Header("Authorization") token: String): Call<ResponseBody>


    @POST("/rest/V1/carts/mine")
    fun generateCartIdWithoutCart(@Header("Authorization") token: String): Call<ResponseBody>


    @DELETE("/rest/V1/carts/mine/items/{path}")
    fun removeFromCart(@Header("Authorization") token: String,@Path("path") string: String): Call<ResponseBody>

    @POST("/rest/V1/carts/mine/items")
    fun addToCart(@Header("Authorization")token: String,@Body jsonObject: JsonObject ): Call<ResponseBody>


    @POST("/rest/V1/carts/mine/shipping-information")
    fun shippingInformation(@Header("Authorization")token: String,@Body jsonObject: JsonObject ): Call<ResponseBody>

    @POST("/rest/V1/carts/mine/payment-information")
    fun paymentInformation(@Header("Authorization")token: String,@Body jsonObject: JsonObject ): Call<ResponseBody>

    companion object {
        const val BASE_URL = "https://magento.232.dengage.shobii.com/"
    }
}