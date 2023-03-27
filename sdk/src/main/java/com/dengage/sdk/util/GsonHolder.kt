package com.dengage.sdk.util

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.google.gson.reflect.TypeToken

object GsonHolder {

    val gson: Gson by lazy {
        GsonBuilder().create()
    }

    fun toJson(source: Any?): String {
        try{
            return gson.toJson(source)
        } catch (ex: Exception)
        { ex.printStackTrace()

        }
        catch (ex: Throwable)
        { ex.printStackTrace()

        }
        catch (ex :IncompatibleClassChangeError)
        { ex.printStackTrace()

        }
        catch (ex :NoSuchFieldError)
        { ex.printStackTrace()

        }
        catch (ex :NoSuchMethodError)
        { ex.printStackTrace()

        }
        catch (ex:java.lang.AssertionError)
        {
            ex.printStackTrace()
        }
        catch (ex:AssertionError)
        {
            ex.printStackTrace()
        }
        return ""
    }

    inline fun <reified T : Any> fromJson(json: String?): T? {
        return try {

            val type = object : TypeToken<T>() {}.type
            return gson.fromJson<T>(json, type)
        } catch (ex: Exception)
        { ex.printStackTrace()
            null
        }
        catch (ex: Throwable)
        { ex.printStackTrace()
            null
        }
        catch (ex :IncompatibleClassChangeError)
        { ex.printStackTrace()
            null
        }
        catch (ex :NoSuchFieldError)
        { ex.printStackTrace()
            null
        }
        catch (ex :NoSuchMethodError)
        { ex.printStackTrace()
            null
        }
        catch (ex:java.lang.AssertionError)
        {
            null
        }
        catch (ex:AssertionError)
        {
            null
        }


    }
}
