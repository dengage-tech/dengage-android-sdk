package com.dengage.android.kotlin.sample

import android.R
import android.app.Application
import android.util.Log
import com.alibaba.fastjson.JSONObject
import com.dengage.android.kotlin.sample.network.RetrofitClient
import com.dengage.android.kotlin.sample.utils.Constants
import com.dengage.sdk.Dengage
import com.dengage.sdk.util.DengageLifecycleTracker
import com.github.chen0040.magento.MagentoClient
import okhttp3.ResponseBody
import org.json.JSONArray
import retrofit2.Call
import retrofit2.Callback
import retrofit2.Response


class App : Application() {
    /*lateinit var dengageManager: DengageManager
*/
    override fun onCreate() {
        super.onCreate()
      /*  val data = HashMap<String, Any>()
        data["page_type"] = "test123"
        Dengage.pageView(data,this)*/
        // to handle application bring to foreground
        registerActivityLifecycleCallbacks(DengageLifecycleTracker())

       /* dengageManager = DengageManager
            .getInstance(applicationContext)
            .setLogStatus(true)
            .setFirebaseIntegrationKey(Constants.FIREBASE_APP_INTEGRATION_KEY)
            .init()*/

        Dengage.init(
            context = applicationContext,
            firebaseIntegrationKey = Constants.FIREBASE_APP_INTEGRATION_KEY
        )
        Dengage.setLogStatus(true)


        Dengage.inAppLinkConfiguration("www.chaitanyamunje.com")
      /*  val filter = IntentFilter(com.dengage.sdk.util.Constants.PUSH_ACTION_CLICK_EVENT)
        registerReceiver(
            PushNotificationReceiver(),
            filter
        )*/

        val magento_site_url = "https://magento.232.dengage.shobii.com"
        val username = "shoaib_shobii@yahoo.com"
        val password = "Shobii**"
        val client = MagentoClient(magento_site_url)
        val thread = Thread {
            try {
                Constants.mToken = client.loginAsClient(username, password)


                Log.d("oops", Constants.mToken)

                val call: Call<ResponseBody> = RetrofitClient.Companion.instance!!.getMyApi().generateCartIdWithCart(
                    "Bearer "+Constants.mToken)
                call.enqueue(object : Callback<ResponseBody> {
                    override fun onResponse(call: Call<ResponseBody>, response: Response<ResponseBody>) {
                       var aa=response.body()?.string()
                        Log.d("oops","dasdhere ${response.body()?.string()}")
                        try {
                            var bodyJson = org.json.JSONObject(aa)
                            if (bodyJson != null) {
                                Constants.cartId = bodyJson.getString("id")

                               var jsonArray: JSONArray = bodyJson.getJSONArray("items")
                                if(jsonArray.length()>0)
                                {
                                    Constants.itemId=jsonArray.getJSONObject(0).getString("item_id")
                                }
                            }
                        }
                        catch (e:java.lang.Exception )
                        {
e.printStackTrace()

                            callCartApi()
                        }
                    }

                    override fun onFailure(call: Call<ResponseBody>, t: Throwable) {
                        Log.d("oops","there")
                    }


                })
            } catch (e: java.lang.Exception) {
                e.printStackTrace()
            }
        }

        thread.start()

    }


    fun callCartApi()
    {
        try{
        val call: Call<ResponseBody> = RetrofitClient.Companion.instance!!.getMyApi().generateCartIdWithoutCart(
            "Bearer "+Constants.mToken)
        call.enqueue(object : Callback<ResponseBody> {
            override fun onResponse(call: Call<ResponseBody>, response: Response<ResponseBody>) {
                var aa=response.body()?.string()
                Log.d("oops","dasdhere ${response.body()?.string()}")
                try {
                    Constants.cartId = aa.toString()

                }
                catch (e:java.lang.Exception )
                {
                    e.printStackTrace()

                    callCartApi()
                }
            }

            override fun onFailure(call: Call<ResponseBody>, t: Throwable) {
                Log.d("oops","there")
            }


        })
    } catch (e: java.lang.Exception) {
        e.printStackTrace()
    }
    }

}