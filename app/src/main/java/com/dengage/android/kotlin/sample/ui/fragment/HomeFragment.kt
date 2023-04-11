package com.dengage.android.kotlin.sample.ui.fragment

import android.util.Log
import android.widget.Toast
import androidx.navigation.fragment.findNavController
import com.dengage.android.kotlin.sample.R
import com.dengage.android.kotlin.sample.databinding.FragmentHomeBinding
import com.dengage.android.kotlin.sample.network.RetrofitClient
import com.dengage.android.kotlin.sample.ui.base.BaseDataBindingFragment
import com.dengage.android.kotlin.sample.utils.Constants
import com.dengage.sdk.Dengage
import com.google.gson.JsonObject
import com.google.gson.JsonParser
import okhttp3.ResponseBody
import org.json.JSONArray
import org.json.JSONObject
import retrofit2.Call
import retrofit2.Callback
import retrofit2.Response


class HomeFragment : BaseDataBindingFragment<FragmentHomeBinding>() {

    override fun getLayoutRes(): Int {
        return R.layout.fragment_home
    }

    override fun init() {
        sendPageView("home")

        binding.btnDeviceInfo.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToDeviceInfo())
        //Dengage.getLastPushPayload()
//Dengage.setDeviceId("sdss")
        }

        binding.btnUserPermission.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToUserPermission())
        }

        binding.btnContactKey.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToContactKey())
        }

        binding.btnCountry.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToCountry())
        }

        binding.btnInboxMessages.setOnClickListener {
          //  findNavController().navigate(HomeFragmentDirections.actionHomeToInboxMessages())
        }

        binding.btnCustomEvents.setOnClickListener {
            findNavController().navigate(HomeFragmentDirections.actionHomeToCustomEvent())
        }

        binding.btnInAppMessage.setOnClickListener {
          //  findNavController().navigate(HomeFragmentDirections.actionHomeToInAppMessage())
            Dengage.sendDeviceIdToServer("V1/dengage/sync/mobile/customerData",Constants.mToken)

        }

        binding.btnRealTimeInAppMessage.setOnClickListener {

            if(Constants.itemId.isNotEmpty()) {
                // findNavController().navigate(HomeFragmentDirections.actionHomeToRealTimeInAppMessage())
                var jsonObject = JSONObject()
                jsonObject.put("username", "shoaib_shobii@yahoo.com")
                jsonObject.put("password", "Shobii**")
                val call: Call<ResponseBody> =
                    RetrofitClient.Companion.instance!!.getMyApi().removeFromCart(
                        "Bearer " + Constants.mToken, Constants.itemId
                    )
                call.enqueue(object : Callback<ResponseBody> {
                    override fun onResponse(
                        call: Call<ResponseBody>,
                        response: Response<ResponseBody>
                    ) {
                        Log.d("oops", "here ${response.body()?.string()}")
                        Constants.itemId=""

                        Toast.makeText(activity,"Removed from cart",Toast.LENGTH_SHORT).show()


                    }

                    override fun onFailure(call: Call<ResponseBody>, t: Throwable) {
                        Log.d("oops", "there")
                    }


                })
            }
            else
            {
                Toast.makeText(activity,"You need to add to cart first",Toast.LENGTH_SHORT).show()
            }
        }

        binding.btnTags.setOnClickListener {
            //findNavController().navigate(HomeFragmentDirections.actionHomeToTags())

            var mainObject = JSONObject()
            var cartItemObj = JSONObject()
            var productOptionsObj = JSONObject()
            var extensionAttributes = JSONObject()
            var configurableitemOptions = JSONArray()
            var extensionAttributesMain = JSONObject()

            mainObject.put("cartItem",cartItemObj)

             cartItemObj.put("sku","WS12")

            cartItemObj.put("qty","1")

            cartItemObj.put("quote_id",Constants.cartId)

             cartItemObj.put("product_option",productOptionsObj)

            productOptionsObj.put("extension_attributes",extensionAttributes)

            extensionAttributes.put("configurable_item_options",configurableitemOptions)
            var item1 = JSONObject()
            var item2 = JSONObject()
            item1.put("option_id","141")
            item1.put("option_value",169)

            item2.put("option_id","93")
            item2.put("option_value",56)

            configurableitemOptions.put(item1 )
            configurableitemOptions.put(item2)

            cartItemObj.put("extension_attributes",extensionAttributesMain)

            val jsonParser = JsonParser()
            val gsonObject = jsonParser.parse(mainObject.toString()) as JsonObject
            val call: Call<ResponseBody> = RetrofitClient.Companion.instance!!.getMyApi().addToCart(
                "Bearer "+Constants.mToken,gsonObject)
            call.enqueue(object : Callback<ResponseBody> {
                override fun onResponse(call: Call<ResponseBody>, response: Response<ResponseBody>) {
                    var aa=response.body()?.string()
                    Log.d("oops","dasdhere ${response.body()?.string()}")
                    try {
                        var bodyJson = org.json.JSONObject(aa)
                        if (bodyJson != null) {
                            Constants.itemId = bodyJson.getString("item_id")
                            Toast.makeText(activity,"Added to cart",Toast.LENGTH_SHORT).show()

                        }
                    }
                    catch (e:java.lang.Exception )
                    {
                        e.printStackTrace()
                    }

                }

                override fun onFailure(call: Call<ResponseBody>, t: Throwable) {
                    Log.d("oops","there")
                }


            })
        }

        binding.btnDengageTestPage.setOnClickListener {
            //Dengage.showTestPage(requireActivity())
            if (Constants.itemId.isNotEmpty()) {
                Toast.makeText(activity, "Placing order", Toast.LENGTH_SHORT).show()

                var mainObject = JSONObject()
                var addressInformation = JSONObject()
                var shipping_address = JSONObject()
                var street = JSONArray()


                mainObject.put("addressInformation", addressInformation)

                addressInformation.put("shipping_address", shipping_address)

                shipping_address.put("firstname", "Shoaib")

                shipping_address.put("lastname", "Raza")

                shipping_address.put("street", street)

                street.put("Saddar")

                shipping_address.put("city", "Karachi")

                shipping_address.put("region", "Sindh")

                shipping_address.put("postcode", "73500")

                shipping_address.put("country_id", "PK")

                shipping_address.put("telephone", "923233340638")

                shipping_address.put("email", "shoaib_shobii@yahoo.com")


                addressInformation.put("shipping_method_code", "flatrate")

                addressInformation.put("shipping_carrier_code", "flatrate")

                mainObject.put("cartId", Constants.cartId)


                val jsonParser = JsonParser()
                val gsonObject = jsonParser.parse(mainObject.toString()) as JsonObject
                val call: Call<ResponseBody> =
                    RetrofitClient.Companion.instance!!.getMyApi().shippingInformation(
                        "Bearer " + Constants.mToken, gsonObject
                    )
                call.enqueue(object : Callback<ResponseBody> {
                    override fun onResponse(
                        call: Call<ResponseBody>,
                        response: Response<ResponseBody>
                    ) {
                        //   var aa=response.body()?.string()
                        Log.d("oops", "dasdhere ${response.body()?.string()}")
                        /* try {
                        var bodyJson = org.json.JSONObject(aa)
                        if (bodyJson != null) {
                           // Constants.itemId = bodyJson.getString("item_id")
                           // Toast.makeText(activity,"Added to cart",Toast.LENGTH_SHORT).show()

                        }
                    }
                    catch (e:java.lang.Exception )
                    {
                        e.printStackTrace()
                    }*/

                        callFinalApi()

                    }

                    override fun onFailure(call: Call<ResponseBody>, t: Throwable) {
                        Log.d("oops", "there")
                    }


                })
            }
            else{
                Toast.makeText(activity,"Cart is empty",Toast.LENGTH_SHORT).show()

            }
        }



    }

    fun callFinalApi()
    {
        var mainObject = JSONObject()
        var paymentMethod = JSONObject()
        var billingAddress = JSONObject()
        var street = JSONArray()


        mainObject.put("paymentMethod",paymentMethod)

        paymentMethod.put("method","checkmo")



        mainObject.put("billingAddress",billingAddress)

        billingAddress.put("firstname","Shoaib")

        billingAddress.put("lastname","Raza")

        billingAddress.put("street",street)

        street.put("Saddar")

        billingAddress.put("city","Karachi")

        billingAddress.put("region","Sindh")

        billingAddress.put("postcode","73500")

        billingAddress.put("country_id","PK")

        billingAddress.put("telephone","923233340638")

        mainObject.put("email","shoaib_shobii@yahoo.com")



        mainObject.put("cartId",Constants.cartId)


        val jsonParser = JsonParser()
        val gsonObject = jsonParser.parse(mainObject.toString()) as JsonObject
        val call: Call<ResponseBody> = RetrofitClient.Companion.instance!!.getMyApi().paymentInformation(
            "Bearer "+Constants.mToken,gsonObject)
        call.enqueue(object : Callback<ResponseBody> {
            override fun onResponse(call: Call<ResponseBody>, response: Response<ResponseBody>) {
                //   var aa=response.body()?.string()
                Log.d("oops","dasdhere ${response.body()?.string()}")

                Toast.makeText(activity,"Order placed",Toast.LENGTH_SHORT).show()

                /* try {
                     var bodyJson = org.json.JSONObject(aa)
                     if (bodyJson != null) {
                        // Constants.itemId = bodyJson.getString("item_id")
                        // Toast.makeText(activity,"Added to cart",Toast.LENGTH_SHORT).show()

                     }
                 }
                 catch (e:java.lang.Exception )
                 {
                     e.printStackTrace()
                 }*/

            }

            override fun onFailure(call: Call<ResponseBody>, t: Throwable) {
                Log.d("oops","there")
            }


        })
    }

}